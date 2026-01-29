# ---------- multiprocessing FIX (MUST be first) ----------
import multiprocessing as mp
mp.set_start_method("fork", force=True)

# ---------- imports ----------
from fastapi import FastAPI, UploadFile, File, Body, HTTPException
from fastapi.responses import JSONResponse
from pydantic import BaseModel
from typing import List, Dict
import pandas as pd
import io
from pyirt import irt

# ---------- app ----------
app = FastAPI(title="IRT 2PL Analysis Service")

@app.get("/")
def root():
    return {"status": "IRT service running"}

# ---------- helpers ----------
def categorize_by_mean_std(value, mean, std, low, high, mid):
    if value < mean - std:
        return low
    elif value > mean + std:
        return high
    return mid


def run_irt_pipeline_json(df: pd.DataFrame) -> dict:
    if "person_id" not in df.columns:
        raise HTTPException(status_code=400, detail="person_id column is required")

    item_columns = [c for c in df.columns if c.startswith("Q")]
    if not item_columns:
        raise HTTPException(status_code=400, detail="No Q* item columns found")

    # ---------- Wide → Long ----------
    long_df = pd.melt(
        df,
        id_vars=["person_id"],
        value_vars=item_columns,
        var_name="item_id",
        value_name="response"
    )

    long_df["item_num_id"] = long_df["item_id"].str.replace("Q", "").astype(int)

    data_for_irt = list(
        zip(long_df["person_id"], long_df["item_num_id"], long_df["response"])
    )

    # ---------- Fit 2PL ----------
    item_param_raw, person_param_raw = irt(
        data_src=data_for_irt,
        dao_type="memory",
        alpha_bnds=[0.2, 3.0],
        beta_bnds=[-4, 4],
        theta_bnds=[-4, 4],
        max_iter=50,
        tol=0.001,
        nargout=2
    )

    # ---------- Convert outputs ----------
    item_param = (
        pd.DataFrame.from_dict(item_param_raw, orient="index")
        .reset_index()
        .rename(columns={"index": "item_id"})
    )

    person_param = (
        pd.DataFrame.from_dict(person_param_raw, orient="index", columns=["ability"])
        .reset_index()
        .rename(columns={"index": "person_id"})
    )

    # ---------- Categorization ----------
    beta_mean, beta_std = item_param["beta"].mean(), item_param["beta"].std()
    alpha_mean, alpha_std = item_param["alpha"].mean(), item_param["alpha"].std()
    theta_mean, theta_std = person_param["ability"].mean(), person_param["ability"].std()

    item_param["difficulty_category"] = item_param["beta"].apply(
        lambda x: categorize_by_mean_std(x, beta_mean, beta_std,
                                         "Too Easy", "Difficult", "Normal")
    )

    item_param["discrimination_category"] = item_param["alpha"].apply(
        lambda x: categorize_by_mean_std(x, alpha_mean, alpha_std,
                                         "Low", "High", "Normal")
    )

    person_param["ability_category"] = person_param["ability"].apply(
        lambda x: categorize_by_mean_std(x, theta_mean, theta_std,
                                         "Weak", "Too_Strong", "Normal")
    )

    # ---------- Merge ability back ----------
    df_out = df.merge(
        person_param,
        on="person_id",
        how="left"
    )

    ability_label_map = {
        "Weak": "Activity too hard",
        "Normal": "Normal",
        "Too_Strong": "Activity too easy"
    }

    df_out["ability_interpretation"] = df_out.apply(
        lambda r: f"{ability_label_map[r['ability_category']]} ({r['ability']:.2f})",
        axis=1
    )

    # ---------- JSON result ----------
    return {
        "items": item_param.sort_values("item_id").to_dict(orient="records"),
        "persons": person_param.sort_values("person_id").to_dict(orient="records"),
        "responses": df_out.to_dict(orient="records")
    }

# ---------- request model ----------
class IRTJsonRequest(BaseModel):
    data: List[Dict[str, int]]

# ---------- endpoints ----------

@app.post("/fit_2pl_json")
async def fit_2pl_json(payload: IRTJsonRequest):
    df = pd.DataFrame(payload.data)
    result = run_irt_pipeline_json(df)
    return JSONResponse(content=result)


@app.post("/fit_2pl_file")
async def fit_2pl_file(file: UploadFile = File(...)):
    content = await file.read()
    df = pd.read_csv(io.BytesIO(content))
    result = run_irt_pipeline_json(df)
    return JSONResponse(content=result)


@app.post("/fit_2pl_text")
async def fit_2pl_text(csv_text: str = Body(..., media_type="text/plain")):
    df = pd.read_csv(io.StringIO(csv_text))
    result = run_irt_pipeline_json(df)
    return JSONResponse(content=result)

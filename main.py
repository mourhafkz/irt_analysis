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
from io import StringIO


# ---------- app ----------
app = FastAPI(title="IRT 2PL Analysis Service")

@app.get("/")
def root():

    text = """
    person_id,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1
    2,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1
    3,1,0,1,1,1,0,0,1,1,1,0,1,1,1,1,0,1,1,1,1
    4,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1
    5,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0
    6,0,0,1,1,1,0,0,0,1,1,0,0,1,0,1,1,0,1,1,1
    7,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1
    8,1,1,1,1,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,0
    9,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,0,1,0,0,1
    10,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,1,1
    11,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,0,1,1,1
    13,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,0,0,1,1
    14,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    15,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1
    16,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1
    17,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0
    18,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,0
    20,1,1,1,0,0,0,1,0,1,0,0,0,0,1,0,0,1,0,0,0
    22,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1
    24,1,0,1,0,1,1,0,1,1,1,0,1,1,0,1,0,1,1,1,1
    """
    df = pd.read_csv(StringIO(text))

    item_columns = [col for col in df.columns if col.startswith("Q")]
    df["total_score"] = df[item_columns].sum(axis=1)

    print("=== Total Score Summary ===")
    print(df["total_score"].describe())

    # ===============================
    # PART 2: 2PL IRT FITTING
    # ===============================

    # Convert wide → long format for pyirt
    long_df = pd.melt(
        df,
        id_vars=["person_id"],
        value_vars=item_columns,
        var_name="item_id",
        value_name="response"
    )
    long_df["item_num_id"] = long_df["item_id"].str.replace("Q", "").astype(int)

    data_for_irt = list(zip(long_df["person_id"], long_df["item_num_id"], long_df["response"]))

    print("Long-format data ready for IRT fitting.")

    # Fit 2PL model
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

    print("2PL model fitted successfully.")

    # Convert outputs to DataFrames
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

    # Categorization helper
    def categorize_by_mean_std(value, mean, std, low, high, mid):
        if value < mean - std:
            return low
        elif value > mean + std:
            return high
        else:
            return mid

    # Item difficulty
    beta_mean = item_param["beta"].mean()
    beta_std = item_param["beta"].std()
    item_param["categorized_difficulty"] = item_param["beta"].apply(
        lambda x: categorize_by_mean_std(x, beta_mean, beta_std, "Too Easy", "Difficult", "Normal")
    )

    # Item discrimination
    alpha_mean = item_param["alpha"].mean()
    alpha_std = item_param["alpha"].std()
    item_param["categorized_discrimination"] = item_param["alpha"].apply(
        lambda x: categorize_by_mean_std(x, alpha_mean, alpha_std, "Low Discrimination", "High Discrimination",
                                         "Normal Discrimination")
    )

    # Student ability
    theta_mean = person_param["ability"].mean()
    theta_std = person_param["ability"].std()
    person_param["categorized_ability"] = person_param["ability"].apply(
        lambda x: categorize_by_mean_std(x, theta_mean, theta_std, "Weak", "Too_Strong", "Normal")
    )

    # Merge abilities back to original df
    df = df.merge(
        person_param[["person_id", "ability", "categorized_ability"]],
        on="person_id",
        how="left"
    )

    ability_label_map = {
        "Weak": "Low ability",
        "Normal": "Normal",
        "Too_Strong": "High ability"
    }

    df["final_ability_display"] = df.apply(
        lambda r: f"{ability_label_map[r['categorized_ability']]} ({r['ability']:.2f})"
        if pd.notna(r["ability"]) else "",
        axis=1
    )

    print(df)

    return {"status": "IRT service running"}



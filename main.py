# ---------- multiprocessing FIX (MUST be first) ----------
import multiprocessing as mp
mp.set_start_method("fork", force=True)

# ---------- imports ----------
from fastapi import FastAPI, UploadFile, File, Body, Request, HTTPException
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

    # text = """
    # person_id,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20
    # 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1
    # 2,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1
    # 3,1,0,1,1,1,0,0,1,1,1,0,1,1,1,1,0,1,1,1,1
    # 4,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1
    # 5,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0
    # 6,0,0,1,1,1,0,0,0,1,1,0,0,1,0,1,1,0,1,1,1
    # 7,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1
    # 8,1,1,1,1,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,0
    # 9,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,0,1,0,0,1
    # 10,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,1,1,1
    # 11,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,0,0,1,1,1
    # 13,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,0,0,1,1
    # 14,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    # 15,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1
    # 16,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1
    # 17,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0
    # 18,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,0
    # 20,1,1,1,0,0,0,1,0,1,0,0,0,0,1,0,0,1,0,0,0
    # 22,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1
    # 24,1,0,1,0,1,1,0,1,1,1,0,1,1,0,1,0,1,1,1,1
    # """
    # df = pd.read_csv(StringIO(text))
    #
    # item_columns = [col for col in df.columns if col.startswith("Q")]
    # df["total_score"] = df[item_columns].sum(axis=1)
    #
    # print("=== Total Score Summary ===")
    # print(df["total_score"].describe())
    #
    # # ===============================
    # # PART 2: 2PL IRT FITTING
    # # ===============================
    #
    # # Convert wide → long format for pyirt
    # long_df = pd.melt(
    #     df,
    #     id_vars=["person_id"],
    #     value_vars=item_columns,
    #     var_name="item_id",
    #     value_name="response"
    # )
    # long_df["item_num_id"] = long_df["item_id"].str.replace("Q", "").astype(int)
    #
    # data_for_irt = list(zip(long_df["person_id"], long_df["item_num_id"], long_df["response"]))
    #
    # print("Long-format data ready for IRT fitting.")
    #
    # # Fit 2PL model
    # item_param_raw, person_param_raw = irt(
    #     data_src=data_for_irt,
    #     dao_type="memory",
    #     alpha_bnds=[0.2, 3.0],
    #     beta_bnds=[-4, 4],
    #     theta_bnds=[-4, 4],
    #     max_iter=50,
    #     tol=0.001,
    #     nargout=2
    # )
    #
    # print("2PL model fitted successfully.")
    #
    # # Convert outputs to DataFrames
    # item_param = (
    #     pd.DataFrame.from_dict(item_param_raw, orient="index")
    #     .reset_index()
    #     .rename(columns={"index": "item_id"})
    # )
    # person_param = (
    #     pd.DataFrame.from_dict(person_param_raw, orient="index", columns=["ability"])
    #     .reset_index()
    #     .rename(columns={"index": "person_id"})
    # )
    #
    # # Categorization helper
    # def categorize_by_mean_std(value, mean, std, low, high, mid):
    #     if value < mean - std:
    #         return low
    #     elif value > mean + std:
    #         return high
    #     else:
    #         return mid
    #
    # # Item difficulty
    # beta_mean = item_param["beta"].mean()
    # beta_std = item_param["beta"].std()
    # item_param["categorized_difficulty"] = item_param["beta"].apply(
    #     lambda x: categorize_by_mean_std(x, beta_mean, beta_std, "Too Easy", "Difficult", "Normal")
    # )
    #
    # # Item discrimination
    # alpha_mean = item_param["alpha"].mean()
    # alpha_std = item_param["alpha"].std()
    # item_param["categorized_discrimination"] = item_param["alpha"].apply(
    #     lambda x: categorize_by_mean_std(x, alpha_mean, alpha_std, "Low Discrimination", "High Discrimination",
    #                                      "Normal Discrimination")
    # )
    #
    # # Student ability
    # theta_mean = person_param["ability"].mean()
    # theta_std = person_param["ability"].std()
    # person_param["categorized_ability"] = person_param["ability"].apply(
    #     lambda x: categorize_by_mean_std(x, theta_mean, theta_std, "Weak", "Too_Strong", "Normal")
    # )
    #
    # # Merge abilities back to original df
    # df = df.merge(
    #     person_param[["person_id", "ability", "categorized_ability"]],
    #     on="person_id",
    #     how="left"
    # )
    #
    # ability_label_map = {
    #     "Weak": "Low ability",
    #     "Normal": "Normal",
    #     "Too_Strong": "High ability"
    # }
    #
    # df["final_ability_display"] = df.apply(
    #     lambda r: f"{ability_label_map[r['categorized_ability']]} ({r['ability']:.2f})"
    #     if pd.notna(r["ability"]) else "",
    #     axis=1
    # )
    #
    # print(df)

    return {"status": "IRT service running"}


def categorize(value, mean, std, low, high, mid):
    if value < mean - std:
        return low
    if value > mean + std:
        return high
    return mid


def generate_teacher_note(difficulty, discrimination, flag):
    if flag:
        return "Item behavior conflicts with student performance. Review wording or distractors."
    if discrimination == "Low":
        return "Item does not differentiate well between students. Consider revising."
    if difficulty == "Difficult":
        return "Consider scaffolding or pre-teaching this skill."
    if difficulty == "Too Easy":
        return "Useful as a warm-up, but limited diagnostic value."
    return "Item functioning as expected."


def build_item_feedback(item_df: pd.DataFrame):
    EMPIRICAL_DIFFICULTY_THRESHOLD = 0.5
    EMPIRICAL_EASY_THRESHOLD = 0.9

    beta_mean, beta_std = item_df.beta.mean(), item_df.beta.std()
    alpha_mean, alpha_std = item_df.alpha.mean(), item_df.alpha.std()

    results = []

    for _, row in item_df.iterrows():
        difficulty = categorize(
            row.beta, beta_mean, beta_std,
            "Too Easy", "Difficult", "Normal"
        )

        discrimination = categorize(
            row.alpha, alpha_mean, alpha_std,
            "Low", "High", "Normal"
        )

        flag = None
        # <-- Fix is here: use row["mean"] instead of row.mean
        if difficulty == "Too Easy" and row["mean"] < EMPIRICAL_DIFFICULTY_THRESHOLD:
            flag = "Problematic - Expected Easy, Empirically Difficult"
        if difficulty == "Difficult" and row["mean"] > EMPIRICAL_EASY_THRESHOLD:
            flag = "Problematic - Expected Difficult, Empirically Easy"

        results.append({
            "item_id": f"Q{int(row.item_id)}",
            "difficulty": {
                "beta": round(row.beta, 2),
                "label": difficulty,
                "empirical_mean": round(row["mean"], 2),
                "flag": flag
            },
            "discrimination": {
                "alpha": round(row.alpha, 2),
                "label": discrimination
            },
            "teacher_note": generate_teacher_note(
                difficulty, discrimination, flag
            )
        })

    return results


# ------------------ Endpoint ------------------
@app.post("/irt/analyze")
async def analyze_irt(request: Request):
    try:
        payload = await request.json()
    except Exception:
        raise HTTPException(status_code=400, detail="Invalid JSON")

    # ---- extract ----
    exercise_uuid = payload.get("exercise_uuid")
    matrix = payload.get("matrix")

    if not exercise_uuid or not matrix:
        raise HTTPException(status_code=400, detail="Missing exercise_uuid or matrix")

    columns = matrix.get("columns")
    rows = matrix.get("rows")

    if not columns or not rows:
        raise HTTPException(status_code=400, detail="Matrix missing columns or rows")

    # ---- build DataFrame (CSV-equivalent) ----
    try:
        df = pd.DataFrame(rows, columns=columns)
        df = df.astype(int)
    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Failed to build matrix: {e}")

    if df.empty or len(df) < 5:
        raise HTTPException(status_code=400, detail="Not enough data for IRT")

    print(df)
    item_columns = [c for c in df.columns if c.startswith("Q")]

    long_df = pd.melt(
        df,
        id_vars=["person_id"],
        value_vars=item_columns,
        var_name="item_id",
        value_name="response"
    )

    long_df["item_num_id"] = long_df["item_id"].str.replace("Q", "").astype(int)

    data_for_irt = list(
        zip(long_df.person_id, long_df.item_num_id, long_df.response)
    )

    item_param_raw, _ = irt(
        data_src=data_for_irt,
        dao_type="memory",
        nargout=2
    )

    item_df = (
        pd.DataFrame.from_dict(item_param_raw, orient="index")
        .reset_index()
        .rename(columns={"index": "item_id"})
    )

    empirical_means = df[item_columns].mean().reset_index()
    empirical_means.columns = ["item_id", "mean"]
    empirical_means["item_id"] = empirical_means["item_id"].str.replace("Q", "").astype(int)

    item_df = item_df.merge(empirical_means, on="item_id")

    high_corr_insights = build_item_insights(df, item_columns)

    return {
        "exercise_uuid": exercise_uuid,
        "items": build_item_feedback(item_df),
        "insights_row": {
            "insights": high_corr_insights,
            "correlation_threshold": 0.6,
            "note": "Insights highlight items that may be related or interesting for review."
        }
    }


def build_item_insights(df, item_columns, corr_threshold=0.6):
    """
    Generate insights for items, including correlations and future extension.
    Each insight contains: type, involved items, strength, explanation, stats, and actions.
    """
    corr = df[item_columns].corr()
    insights = []
    color_group_id = 0

    for i in range(len(item_columns)):
        for j in range(i + 1, len(item_columns)):
            c = corr.iloc[i, j]
            if abs(c) >= corr_threshold:
                color_group_id += 1
                insights.append({
                    "insight_id": f"corr_{item_columns[i]}_{item_columns[j]}",
                    "type": "correlation",
                    "items": [item_columns[i], item_columns[j]],
                    "strength": round(c, 2),
                    "severity": "high" if abs(c) > 0.75 else "medium",
                    "ui": {
                        "color_group": color_group_id
                    },
                    "explanation": {
                        "short": "Possibly related",
                        "long": (
                            f"{item_columns[i]} and {item_columns[j]} responses are highly correlated. "
                            "Students tend to answer them similarly or right after each other, "
                            "suggesting overlapping skills"
                        )
                    },
                    "stats": {
                        "correlation": round(c, 2)
                    },
                    "actions": [
                        "Consider replacing one item if the effect is not intended.",
                        "This is also highly likely to be a misinterpretation when the number of attempts is small (<300)."
                    ]
                })
    return insights

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

#!/usr/bin/env python
# coding: utf-8

# Create FastAPI
# 
# DSSA 5201 - Machine Learning Fundamentals
#  
# Poorva Patel 5/2/24

# In[ ]:


import pickle
from fastapi import FastAPI, HTTPException, Depends, Header
from pydantic import BaseModel
import uvicorn
import asyncio
import nest_asyncio
from fastapi.responses import PlainTextResponse

app = FastAPI()

@app.get("/")
async def read_root():
    return {"Hello": "World"}

@app.get("/favicon.ico")
async def favicon():
    return PlainTextResponse("")

#loop for notebook
nest_asyncio.apply()

# Load the model
with open('model.pickle', 'rb') as model_file:
    model = pickle.load(model_file)

class PredictionInput(BaseModel):
    features: list  

# Dependency to validate API key
def authenticate_api_key(api_key: str = Header(...)):
    valid_api_keys = ["1234", "5678"]
    if api_key not in valid_api_keys:
        raise HTTPException(status_code=403, detail="Invalid API key")
    return api_key

@app.post("/predict/")
def make_prediction(input_data: PredictionInput, api_key: str = Depends(authenticate_api_key)):
    try:
        prediction = model.predict([input_data.features])[0]  
        return {"prediction": prediction}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)


# In[ ]:


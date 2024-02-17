from fastapi import FastAPI
from pydantic import BaseModel

class Item(BaseModel):
    id: str

app = FastAPI()

@app.get("/")
async def root() -> Item:
    return Item(id="Value")

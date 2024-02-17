from fastapi import FastAPI
from pydantic import BaseModel

LIKES = 0

class Item(BaseModel):
    id: str

class Likes(BaseModel):
    likes: int

app = FastAPI()

@app.get("/")
async def root() -> Item:
    return Item(id="Andy")

@app.post("/like")
async def likes() -> Likes:
    global LIKES
    LIKES += 1
    return Likes(likes=LIKES)

@app.get("/like")
async def likes() -> Likes:
    global LIKES
    return Likes(likes=LIKES)

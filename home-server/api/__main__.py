import sqlite3
from contextlib import asynccontextmanager

from fastapi import FastAPI
from pydantic import BaseModel

LIKES = 0


class Post(BaseModel):
    title: str
    body: str


class Item(BaseModel):
    id: str


class Likes(BaseModel):
    likes: int


connection = None


@asynccontextmanager
async def lifespan(app: FastAPI):
    global connection
    connection = sqlite3.connect("blog.db")
    cursor = connection.cursor()
    cursor.execute(
        """
        CREATE TABLE IF NOT EXISTS
            likes (id INTEGER UNIQUE, like INTEGER);
        """
    )
    yield
    connection.commit()
    connection.close()


app = FastAPI(lifespan=lifespan)


@app.get("/")
async def root() -> Item:
    return Item(id="Andy")


@app.post("/like")
async def post_likes() -> Likes:
    global connection
    cursor = connection.cursor()
    result = cursor.execute("SELECT like FROM likes WHERE id=1;")
    rows = result.fetchall()
    if len(rows) == 0:
        likes = 0
    else:
        likes = rows[0][0]
    likes += 1
    cursor.execute("INSERT OR REPLACE INTO likes (id, like) VALUES (1, ?);", (likes,))
    connection.commit()
    return Likes(likes=likes)


@app.get("/like")
async def get_likes() -> Likes:
    global connection
    cursor = connection.cursor()
    result = cursor.execute("SELECT like FROM likes WHERE id=1;")
    rows = result.fetchall()
    if len(rows) == 0:
        likes = 0
    else:
        likes = rows[0][0]
    return Likes(likes=likes)

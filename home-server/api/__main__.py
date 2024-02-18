import sqlite3
from contextlib import asynccontextmanager

from fastapi import FastAPI

from lib import Item, Likes, Post

LIKES = 0


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
    cursor.execute(
        """
        CREATE TABLE IF NOT EXISTS posts
            (id INTEGER PRIMARY KEY, title TEXT, body TEXT);
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


@app.post("/posts")
async def posts(form: Post) -> list[Post]:
    global connection
    cursor = connection.cursor()

    # Insert post
    cursor.execute(
        """INSERT INTO posts
                (title, body) VALUES (?, ?);
        """,
        (form.title, form.body),
    )
    connection.commit()

    # Return all posts
    result = cursor.execute("SELECT title, body FROM posts;")
    rows = result.fetchall()
    return [Post(title=title, body=body) for (title, body) in rows]


@app.post("/post/{id}")
async def post_update(id: int, form: Post) -> list[Post]:
    global connection
    cursor = connection.cursor()

    # Insert post
    cursor.execute(
        """INSERT OR REPLACE INTO posts
                (id, title, body) VALUES (?, ?, ?);
        """,
        (id, form.title, form.body),
    )
    connection.commit()

    # Return all posts
    result = cursor.execute("SELECT title, body FROM posts;")
    rows = result.fetchall()
    return [Post(title=title, body=body) for (title, body) in rows]


@app.get("/posts")
async def posts_get() -> list[Post]:
    global connection
    cursor = connection.cursor()
    result = cursor.execute("SELECT id, title, body FROM posts;")
    rows = result.fetchall()
    return [Post(title=title, body=body, id=id) for (id, title, body) in rows]


@app.get("/edit/{id}")
async def posts_edit(id: int) -> list[Post]:
    global connection
    cursor = connection.cursor()
    result = cursor.execute(
        """SELECT id, title, body
           FROM posts
           WHERE id=?;
        """,
        (id,),
    )
    rows = result.fetchall()
    return [Post(title=title, body=body, id=id) for (id, title, body) in rows]

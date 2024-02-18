import requests
from flask import Flask, render_template, url_for
from flask_pydantic import validate

import lib
from lib import Post

app = Flask(__name__)

API_SERVER = "http://localhost:8000"


def api(uri: str):
    return f"{API_SERVER}{uri}"


@app.route("/")
def index():
    # response = requests.get(f"{API_SERVER}")
    # name = response.json()["id"]
    name = lib.VALUE
    style_href = url_for("static", filename="style.css")
    return render_template("index.html", name=name, style_href=style_href)


@app.post("/like")
def like():
    response = requests.post(api("/like"))
    likes = response.json()["likes"]
    return render_template("likes.html", likes=likes)


@app.get("/likes")
def likes():
    response = requests.get(api("/like"))
    likes = response.json()["likes"]
    return render_template("likes.html", likes=likes)


@app.post("/posts")
@validate()
def posts_post(form: Post):
    response = requests.post(api("/posts"), json=form.dict())
    return render_template("posts.html", posts=response.json())


@app.get("/posts")
def posts_get():
    response = requests.get(api("/posts"))
    return render_template("posts.html", posts=response.json())

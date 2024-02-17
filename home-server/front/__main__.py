import requests
from flask import Flask, render_template, url_for

app = Flask(__name__)

API_SERVER = "http://localhost:8000"


@app.route("/")
def index():
    response = requests.get(f"{API_SERVER}")
    name = response.json()["id"]
    style_href = url_for("static", filename="style.css")
    return render_template("index.html", name=name, style_href=style_href)


@app.route("/like", methods=["POST"])
def like():
    response = requests.post(f"{API_SERVER}/like")
    likes = response.json()["likes"]
    return render_template("likes.html", likes=likes)


@app.route("/likes")
def likes():
    response = requests.get(f"{API_SERVER}/like")
    likes = response.json()["likes"]
    return render_template("likes.html", likes=likes)

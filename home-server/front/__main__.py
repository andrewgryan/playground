import requests
from flask import Flask, url_for, render_template

app = Flask(__name__)

@app.route("/")
def index():
    response = requests.get("http://localhost:8000")
    name = response.json()["id"]
    style_href = url_for("static", filename="style.css")
    return render_template("index.html", name=name, style_href=style_href)

@app.route("/like", methods=["POST"])
def like():
    response = requests.post("http://localhost:8000/like")
    likes = response.json()["likes"]
    return render_template("likes.html", likes=likes)

@app.route("/likes")
def likes():
    response = requests.get("http://localhost:8000/like")
    likes = response.json()["likes"]
    return render_template("likes.html", likes=likes)

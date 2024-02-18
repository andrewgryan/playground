from typing import Optional

from pydantic import BaseModel

VALUE = "Foo"


class Post(BaseModel):
    title: str
    body: str
    id: Optional[int] = None


class Item(BaseModel):
    id: str


class Likes(BaseModel):
    likes: int

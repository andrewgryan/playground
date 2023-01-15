main = putStrLn (makeHtml "Title" "Hello, world!")

makeHtml :: String -> String -> String
makeHtml title body = html_ (head_ (title_ title) <> body_ (h1_ body <> p_ body))

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

h1_ :: String -> String
h1_ = el "h1"

p_ :: String -> String
p_ = el "p"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


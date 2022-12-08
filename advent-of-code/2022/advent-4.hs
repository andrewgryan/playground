import Text.Read
import Data.Maybe
import Data.List.Split

data Pair = Pair Range Range deriving Show
data Range = Range Int Int deriving Show

toPair :: String -> Pair
toPair s =
    let
        parts = fmap fromString (splitOn "," s)
        left = head parts
        right = last parts
    in
    Pair left right

fromString :: String -> Range
fromString s =
    let
        parts = fmap (read::String->Int) (splitOn "-" s)
        left = head parts
        right = last parts
    in
    Range left right
        
solutionPart1 :: String -> Int
solutionPart1 text =
    length (filter overlap (lines text))

solutionPart2 :: String -> Int
solutionPart2 text =
    length (filter overlapAtAll (lines text))

main = do
    text <- readFile "input-4"
    print (solutionPart2 text)
        
overlapAtAll :: String -> Bool
overlapAtAll s =
    let
        pair = toPair s
    in
    case pair of
        Pair left right ->
            inInterval (x left) right || inInterval (y left) right || overlap s
                        
inInterval :: Int -> Range -> Bool
inInterval x (Range left right) =
     (x >= left) && (x <= right)

x :: Range -> Int
x (Range l _) =
    l

y :: Range -> Int
y (Range _ r) =
    r


overlap :: String -> Bool
overlap s =
    let
        pair = toPair s
    in
    case pair of
        Pair left right ->
            inside left right || inside right left


inside :: Range -> Range -> Bool
inside left right =
    case (left, right) of
        ((Range x0 y0), (Range x1 y1)) ->
            (x0 >= x1) && (y0 <= y1)
 

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

main = do
    text <- readFile "input-4"
    print (length (filter overlap (lines text)))
        
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
 

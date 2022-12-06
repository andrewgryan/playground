-- Advent of code day 3
import Text.Read
import Data.Set
import Data.Maybe

score :: Char -> Maybe Int
score c =
    case c of
        'a' -> Just 1
        'b' -> Just 2
        'c' -> Just 3
        'd' -> Just 4
        'e' -> Just 5
        'f' -> Just 6
        'g' -> Just 7
        'h' -> Just 8
        'i' -> Just 9
        'j' -> Just 10
        'k' -> Just 11
        'l' -> Just 12
        'm' -> Just 13
        'n' -> Just 14
        'o' -> Just 15
        'p' -> Just 16
        'q' -> Just 17
        'r' -> Just 18
        's' -> Just 19
        't' -> Just 20
        'u' -> Just 21
        'v' -> Just 22
        'w' -> Just 23
        'x' -> Just 24
        'y' -> Just 25
        'z' -> Just 26
        'A' -> Just 27
        'B' -> Just 28
        'C' -> Just 29
        'D' -> Just 30
        'E' -> Just 31
        'F' -> Just 32
        'G' -> Just 33
        'H' -> Just 34
        'I' -> Just 35
        'J' -> Just 36
        'K' -> Just 37
        'L' -> Just 38
        'M' -> Just 39
        'N' -> Just 40
        'O' -> Just 41
        'P' -> Just 42
        'Q' -> Just 43
        'R' -> Just 44
        'S' -> Just 45
        'T' -> Just 46
        'U' -> Just 47
        'V' -> Just 48
        'W' -> Just 49
        'X' -> Just 50
        'Y' -> Just 51
        'Z' -> Just 52
        _ -> Nothing
                
findDuplicates :: String -> String -> Set Char
findDuplicates left right =
    fromList left `intersection` fromList right

calculate :: String -> Int
calculate s =
    let
        n = length s
        t = n `div` 2
        left = Prelude.take t s
        right = Prelude.take t (reverse s)
        duplicates = toList (findDuplicates left right)
    in
    sum (fmap (fromJust . score) duplicates)

main = do
    text <- readFile "input-3"
    print (sum (fmap calculate (lines text)))

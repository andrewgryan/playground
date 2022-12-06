-- Advent of Code Day 2
import Data.Maybe

data Hand = Rock | Paper | Scissors
data Round = Win | Lose | Draw

fromChar :: Char -> Maybe Hand
fromChar c =
   case c of
     'A' -> Just Rock
     'B' -> Just Paper
     'C' -> Just Scissors
     'X' -> Just Rock
     'Y' -> Just Paper
     'Z' -> Just Scissors
     _ -> Nothing
    
fromHands :: Hand -> Hand -> Round
fromHands opponent you =
    case (opponent, you) of
        (Rock, Rock) -> Draw
        (Rock, Paper) -> Win
        (Rock, Scissors) -> Lose
        (Paper, Rock) -> Lose
        (Paper, Paper) -> Draw
        (Paper, Scissors) -> Win
        (Scissors, Rock) -> Win
        (Scissors, Paper) -> Lose
        (Scissors, Scissors) -> Draw
        
roundScore :: Round -> Int
roundScore outcome =
    case outcome of
        Win -> 6
        Draw -> 3
        Lose -> 0
        
handScore :: Hand -> Int
handScore hand =
    case hand of
        Rock -> 1
        Paper -> 2
        Scissors -> 3
        
totalScore :: Hand -> Hand -> Int
totalScore opponent you =
    roundScore (fromHands opponent you) + handScore you

calculateScore :: String -> Int
calculateScore s =
    let
        opponent = fromJust (fromChar (head s))
        you = fromJust (fromChar (last s))
    in
    totalScore opponent you

main = do
  text <- readFile "input-2"
  let rounds = lines text
  print (sum (fmap calculateScore rounds))

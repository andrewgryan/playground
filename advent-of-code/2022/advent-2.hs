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
        
roundFromChar :: Char -> Maybe Round
roundFromChar c =
   case c of
     'X' -> Just Lose
     'Y' -> Just Draw
     'Z' -> Just Win
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

calculatePart1Score :: String -> Int
calculatePart1Score s =
    let
        opponent = fromJust (fromChar (head s))
        you = fromJust (fromChar (last s))
    in
    totalScore opponent you

strategy :: Hand -> Round -> Hand
strategy opponent round =
    case (opponent, round) of
        (Rock, Win) -> Paper
        (Rock, Draw) -> Rock
        (Rock, Lose) -> Scissors
        (Paper, Win) -> Scissors
        (Paper, Draw) -> Paper
        (Paper, Lose) -> Rock
        (Scissors, Win) -> Rock
        (Scissors, Draw) -> Scissors
        (Scissors, Lose) -> Paper
        
calculatePart2Score :: String -> Int
calculatePart2Score s =
    -- X, Y, Z maps to strategy
    let
        opponent = fromJust (fromChar (head s))
        round = fromJust (roundFromChar (last s))
        you = strategy opponent round
    in
    totalScore opponent you

main = do
  text <- readFile "input-2"
  let rounds = lines text
  print (sum (fmap calculatePart1Score rounds))
  print (sum (fmap calculatePart2Score rounds))

import Data.List.Split (splitOn)
import qualified Data.Maybe as Maybe

type Cycle = Int
type Register = Int
data CPU = CPU Cycle Register deriving Show

boot :: CPU
boot =
  CPU 0 1

advance :: Int -> ([Op], CPU) -> ([Op], CPU) 
advance nticks (ops, cpu) =
  if nticks == 0 then
    (ops, cpu)
  else
    advance (nticks - 1) (advanceOnce (ops, cpu))

advanceOnce :: ([Op], CPU) -> ([Op], CPU)
advanceOnce ([], cpu) =
  ([], tick cpu)
advanceOnce (op:ops, cpu) =
  case op of
    Noop _ ->
      (ops, tick cpu)
    Addx ticks n ->
      if ticks == 0 then
        advanceOnce (ops, store n cpu)
      else if ticks == 1 then
        (Addx 0 n:ops, tick cpu)
      else
        (Addx (ticks - 1) n:ops, tick cpu)
  
store :: Int -> CPU -> CPU
store n (CPU clock register) =
  CPU clock (register + n)

tick :: CPU -> CPU
tick (CPU clock register) =
  CPU (clock + 1) register

data Op
  = Noop Int
  | Addx Int Int
  deriving Show

toOp :: String -> Maybe Op
toOp s =
  case splitOn " " s of
    ["noop"] -> Just (Noop 1)
    ["addx", x] -> Just (Addx 2 (read x :: Int))
    _ -> Nothing

smallProgram :: [String]
smallProgram =
  [ "noop"
  , "addx 3"
  , "addx -5"
  ]

signalStrength :: [Op] -> Int -> Int
signalStrength ops cycle =
  let
    (_, cpu) = advance cycle (ops, boot)
  in
  strength cpu

strength :: CPU -> Int
strength (CPU cycle register) =
  cycle * register

main = do
  text <- readFile "input-10"
  let ops = Maybe.mapMaybe toOp (lines text)
  let cycles = [20, 60, 100, 140, 180, 220]
  let strengths = fmap (signalStrength ops) cycles
  print (sum strengths)

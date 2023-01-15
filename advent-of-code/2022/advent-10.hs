import Data.List.Split (splitOn, chunksOf)
import qualified Data.Maybe as Maybe

type Cycle = Int
type Register = Int
data CPU = CPU Cycle Register deriving Show

type Width = Int
data CRT = CRT Width [Pixel] deriving Show
data Pixel = Lit | Dark deriving Show

turnOnCRT :: CRT
turnOnCRT =
  CRT 40 []
  
toString :: CRT -> String
toString (CRT w pxs) =
  unlines (chunksOf w (fmap pxToChar pxs))
  
pxToChar :: Pixel -> Char
pxToChar Lit = '#'
pxToChar Dark = '.'
 
draw :: CRT -> CPU -> CRT
draw crt cpu =
  if abs (crtX crt - cpuX cpu) <= 1 then
    drawPixel Lit crt
  else
    drawPixel Dark crt
    
drawPixel :: Pixel -> CRT -> CRT
drawPixel pixel (CRT w pixels) =
  CRT w (pixel:pixels)

cpuX :: CPU -> Int
cpuX (CPU _ register) =
  register
  
crtX :: CRT -> Int
crtX (CRT width pixels) =
  length pixels `mod` width

boot :: CPU
boot =
  CPU 0 1

advanceWithCRT :: Int -> ([Op], CPU, CRT) -> ([Op], CPU, CRT) 
advanceWithCRT nticks (ops, cpu, crt) =
  if nticks == 0 then
    (ops, cpu, crt)
  else
    advanceWithCRT (nticks - 1) (advanceOnceWithCRT (ops, cpu, crt))

advanceOnceWithCRT :: ([Op], CPU, CRT) -> ([Op], CPU, CRT)
advanceOnceWithCRT (ops, cpu, crt) =
  let
    (ops', cpu') = advanceOnce (ops, cpu)
  in
  (ops', cpu', draw crt cpu')

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

main :: IO ()
main = do
  text <- readFile "input-10"
  partTwo text
  
partTwo :: String -> IO ()
partTwo text = do
  let ops = Maybe.mapMaybe toOp (lines text)
  let (_, _, crt) = advanceWithCRT 240 (ops, boot, turnOnCRT)
  putStr (toString crt)
  
partOne :: String -> IO ()
partOne text = do
  let ops = Maybe.mapMaybe toOp (lines text)
  let cycles = [20, 60, 100, 140, 180, 220]
  let strengths = fmap (signalStrength ops) cycles
  print (sum strengths)

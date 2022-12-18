import Data.List.Split (splitOn)
import qualified Data.Maybe as Maybe

type Position = (Int, Int)
data Rope = Rope Position Position deriving Show

newRope :: Rope
newRope =
    Rope (0, 0) (0, 0)

move :: Rope -> Instruction -> Rope
move rope (Instruction direction times) =
    multiMove times rope direction

multiMove :: Int -> Rope -> Direction -> Rope
multiMove 0 rope _ =
    rope
multiMove n rope direction =
    multiMove (n - 1) (moveOnce rope direction) direction

moveOnce :: Rope -> Direction -> Rope
moveOnce (Rope head tail) direction =
    let
        head' = translate head (vector direction)
    in
    if closeEnough head' tail then
        Rope head' tail
    else
        Rope head' (snap head' tail)

snap :: Position -> Position -> Position
snap head tail =
    translate tail (normal (head `minus` tail))

normal :: Position -> Position        
normal (x, y) =
    (signum x, signum y)

minus :: Position -> Position -> Position
minus (hx, hy) (tx, ty) =
    (hx - tx, hy - ty)

translate :: (Int, Int) -> (Int, Int) -> (Int, Int)
translate (px, py) (dx, dy) =
    (px + dx, py + dy)

vector :: Direction -> (Int, Int)  
vector d =
    case d of
        U -> (0, 1)
        D -> (0, -1)
        R -> (1, 0)
        L -> (-1, 0)
        
closeEnough :: Position -> Position -> Bool
closeEnough (hx, hy) (tx, ty) =
    (abs (hx - tx) <= 1) && (abs (hy - ty) <= 1)

tailPosition :: Rope -> Position
tailPosition (Rope _ position) =
    position

data Direction
    = U
    | D
    | L
    | R
    deriving Show

data Instruction = Instruction Direction Int deriving Show

toInstruction :: String -> Maybe Instruction
toInstruction s =
    let
      parts = splitOn " " s
    in
    case parts of
        [x, y] -> Just (Instruction (Maybe.fromJust (toDirection x)) (read y :: Int))
        _ -> Nothing

toDirection :: String -> Maybe Direction
toDirection s =
  case s of
    "U" -> Just U
    "D" -> Just D
    "L" -> Just L
    "R" -> Just R
    _ -> Nothing

example :: [String]
example =
  [ "R 4"
  , "U 4"
  , "L 3"
  , "D 1"
  , "R 4"
  , "D 1"
  , "L 5"
  , "R 2"
  ]

solve :: [Instruction] -> Int
solve instructions =
  0

main = do
  let instructions = Maybe.mapMaybe toInstruction example
  print (foldl move newRope instructions)

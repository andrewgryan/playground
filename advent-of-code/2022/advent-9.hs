import Data.List.Split (splitOn)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Position = (Int, Int)
type Rope = [Position]

newRope :: Int -> Rope
newRope n =
    replicate n (0, 0)

move :: (Rope, [Position]) -> Instruction -> (Rope, [Position])
move rope (Instruction direction times) =
    case times of
        0 ->
            rope
        n ->
            move (moveOnce rope direction) (Instruction direction (times - 1))

moveOnce :: (Rope, [Position]) -> Direction -> (Rope, [Position])
moveOnce (rope, tailPositions) direction =
    let
        front = head rope
        end = last rope
        front' = translate front (vector direction)
        pairs = zip rope (tail rope)
        end' = fmap magnet pairs
        rope' = front':end'
    in
    (rope', last rope':tailPositions)

magnet :: (Position, Position) -> Position
magnet (front, end) =
    if closeEnough front end then
        end
    else
        snap front end

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

largerExample :: [String]
largerExample =
  [ "R 5"
  , "U 8"
  , "L 8"
  , "D 3"
  , "R 17"
  , "D 10"
  , "L 25"
  , "U 20"
  ]

main = do
    text <- readFile "input-9"
    let mode = "example"
    let instructions = if mode == "example" then
            Maybe.mapMaybe toInstruction largerExample
        else
            Maybe.mapMaybe toInstruction (lines text)
    -- Part 1: segments = 2
    let segments = 9
    let (rope, tailPositions) = foldl move (newRope segments, [(0, 0)]) instructions
    print (length (Set.fromList tailPositions))

import qualified Data.Maybe as Maybe
import qualified Data.List as List

newtype Tree = Tree Int deriving Show

visible :: [[Tree]] -> (Int, Int) -> Bool
visible trees (i, j) =
  visible1d j (row i trees) || visible1d i (col j trees)

col :: Int -> [[Tree]] -> [Tree]
col i trees =
  List.transpose trees !! i
  
row :: Int -> [[Tree]] -> [Tree]
row i trees =
  trees !! i
  
visible1d :: Int -> [Tree] -> Bool
visible1d i trees =
  let
    (left, right) = splitAt i trees
  in
  case (left, right) of
    ([], []) -> True
    ([], _) -> True -- On an edge
    (ls, [t]) -> True
    (ls, rs) ->
      case rs of
          [] -> True
          [t] -> True
          (t:xs) -> (height t > maximum (fmap height ls)) || (height t > maximum (fmap height xs))

height :: Tree -> Int
height (Tree h) =
  h

fromList :: [String] -> [[Tree]]
fromList =
    fmap toRow
    
shape :: [[Tree]] -> (Int, Int)
shape rows =
    (length rows, length (head rows))
    
indices :: (Int, Int) -> [(Int, Int)]
indices (nx, ny) =
  [(i, j) | i <- [0..nx-1], j <- [0..ny-1]]

toRow :: String -> [Tree]
toRow =
    Maybe.mapMaybe toTree

toTree :: Char -> Maybe Tree
toTree c =
  case c of
    '0' -> Just (Tree 0)
    '1' -> Just (Tree 1)
    '2' -> Just (Tree 2)
    '3' -> Just (Tree 3)
    '4' -> Just (Tree 4)
    '5' -> Just (Tree 5)
    '6' -> Just (Tree 6)
    '7' -> Just (Tree 7)
    '8' -> Just (Tree 8)
    '9' -> Just (Tree 9)
    _ -> Nothing
    
-- Find Trees N/S or E/W in Row/Col


example :: [String]
example =
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]

main = do
  -- Example
  -- let trees = fromList example
  text <- readFile "input-8"
  let trees = fromList (lines text)
  -- print (solutionPart1 trees)
  print (solutionPart2 trees)

solutionPart2 :: [[Tree]] -> Int
solutionPart2 trees =
  maximum (fmap (scenicScore trees) (indices (shape trees)))
  
scenicScore :: [[Tree]] -> (Int, Int) -> Int
scenicScore trees (i, j) =
  let
    tree = trees !! i !! j
  in
  ( view tree (reverse (north trees (i, j)))
  * view tree (south trees (i, j))
  * view tree (east trees (i, j))
  * view tree (reverse (west trees (i, j)))
  )

view :: Tree -> [Tree] -> Int
view tree trees =
  score tree trees 0
  
score :: Tree -> [Tree] -> Int -> Int
score center trees tally =
  case trees of
    [] -> tally
    (t:ts) ->
      if height center > height t then
        score center ts (tally + 1)
      else
        tally + 1

north :: [[Tree]] -> (Int, Int) -> [Tree]
north trees (i, j) =
  take i (col j trees)

south :: [[Tree]] -> (Int, Int) -> [Tree]
south trees (i, j) =
  let
    (_, ny) = shape trees
  in
  (fromEnd (ny - (i + 1)) . col j) trees

east :: [[Tree]] -> (Int, Int) -> [Tree]
east trees (i, j) =
  let
    (nx, _) = shape trees
  in
  (fromEnd (nx - (j + 1)) . row i) trees

west :: [[Tree]] -> (Int, Int) -> [Tree]
west trees (i, j) =
  take j (row i trees)

fromEnd :: Int -> [Tree] -> [Tree]
fromEnd n =
  reverse . take n . reverse

solutionPart1 :: [[Tree]] -> Int
solutionPart1 trees =
  let
      pts = indices (shape trees)
      trees' = filter (visible trees) pts
  in
  length trees'

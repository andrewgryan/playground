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
  -- example
  -- let trees = fromList example
  text <- readFile "input-8"
  let trees = fromList (lines text)
  let pts = indices (shape trees)
  let trees' = filter (visible trees) pts
  print (length pts)
  print (length trees')

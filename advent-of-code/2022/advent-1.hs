import Text.Read
import Data.List

main = do
    text <- readFile "input"
    let items = lines text
    let nums = fmap (\s -> readMaybe s :: Maybe Int) items
    let elves = makeElves nums [] []
    let calories = fmap sum elves
    let maybeMaxCalories = maxi calories
    fmap elemIndex (maxi calories)
    print (maxi calories)
    

solve :: [Int] -> Int
solve calories =
    0
        
indexOf :: Int -> [Int] -> Maybe Int
indexOf n [] = Nothing
indexOf n (x:xs) = Just 0
        
maxi :: [Int] -> Maybe Int
maxi [] = Nothing
maxi (x:xs) = Just (foldl max x xs)
        
makeElves :: [Maybe Int] -> [Int] -> [[Int]] -> [[Int]]
makeElves [] [] c = c
makeElves [] xs c = xs : c
makeElves (m:ms) xs c =
   case m of
    Nothing -> makeElves ms [] (xs : c)
    Just v -> makeElves ms (v : xs) c
  

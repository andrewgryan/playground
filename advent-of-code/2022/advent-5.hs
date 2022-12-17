import Text.Read
import Data.List (reverse, stripPrefix, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Crate = Crate Char deriving (Show)
data Stack = Stack Char [Crate] deriving (Show)
data Instruction = Instruction Int Char Char deriving (Show)
data Scene = Scene [Stack] [Instruction] deriving (Show)

toScene :: String -> Scene
toScene s =
    Scene (parseStacks s) (parseInstructions s)

toStack :: String -> Stack
toStack s =
    case s of
        (x:xs) -> Stack x (((fmap Crate) . (filter notBlank)) xs)
        _ -> Stack ' ' [] -- Too lazy to do a Maybe Stack

parseStacks :: String -> [Stack]
parseStacks =
   (fmap toStack) . (filter isValid) . (fmap reverse) . transpose . take 9 . lines

isValid :: String -> Bool
isValid s =
    not ((hasChar '[' s) || (hasChar ']' s) || ((all isBlank) s))

hasChar :: Char -> String -> Bool
hasChar =
    any . (==)

notBlank :: Char -> Bool
notBlank =
    not . isBlank

isBlank :: Char -> Bool
isBlank = (== ' ')


toInstruction :: String -> Maybe Instruction
toInstruction s =
    let
        words = splitOn " " s
    in
    case words of
        ("move":x:"from":y:"to":z:_) ->
            Just (Instruction (read x :: Int) (head y) (head z))
        _ ->
            Nothing

parseInstructions :: String -> [Instruction]
parseInstructions =
    catMaybes . (fmap toInstruction) . lines

resolve :: Scene -> [Stack]
resolve (Scene stacks instructions) =
   foldl moveCrates stacks instructions

moveCrates :: [Stack] -> Instruction -> [Stack]
moveCrates stacks (Instruction n from to) =
    let
        map = Map.fromList (fmap toTuple stacks)
        fromCrates = Maybe.fromJust (Map.lookup from map)
        toCrates = Maybe.fromJust (Map.lookup to map)
        unloadedCrates = reverse (take n (reverse fromCrates))
        remainingCrates = reverse (drop n (reverse fromCrates))
        map' = Map.insert from remainingCrates map
        map'' = Map.insert to (toCrates ++ unloadedCrates) map'
        newStacks = fmap fromTuple (Map.toList map'')
    in
    newStacks
        
toTuple :: Stack -> (Char, [Crate])
toTuple (Stack c crates) =
    (c, crates)
        
fromTuple :: (Char, [Crate]) -> Stack
fromTuple (c, crates) =
    Stack c crates
        
onTop :: [Stack] -> String
onTop =
    fmap lastCrate . List.sortOn stackKey

stackKey :: Stack -> Char
stackKey (Stack k _) =
    k
        
lastCrate :: Stack -> Char
lastCrate (Stack _ crates) =
    toChar (head crates)
        
toChar :: Crate -> Char
toChar (Crate c) =
    c

main = do
    text <- readFile "input-5"
    print (resolve (toScene text))
    print (onTop (resolve (toScene text)))

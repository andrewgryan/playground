import Text.Read
import Data.List (reverse, stripPrefix, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

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
moveCrates stacks _ =
    stacks


main = do
    text <- readFile "input-5"
    print (resolve (toScene text))

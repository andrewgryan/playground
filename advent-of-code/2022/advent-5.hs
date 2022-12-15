import Text.Read
import Data.List (reverse, stripPrefix, transpose)
import Data.List.Split (splitOn)

data Crate = Crate Char deriving (Show)
data Stack = Stack Char [Crate] deriving (Show)
data Instruction = Instruction String String String deriving (Show)

toStack :: String -> Stack
toStack s =
    case s of
        (x:xs) -> Stack x (((fmap Crate) . (filter notBlank)) xs)
        _ -> Stack ' ' [] -- Too lazy to do a Maybe Stack

parseBoard :: String -> [Stack]
parseBoard =
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
            Just (Instruction x y z)
        _ ->
            Nothing

parseInstructions :: String -> [Maybe Instruction]
parseInstructions =
    (fmap toInstruction) . lines

main = do
    text <- readFile "input-5"
    print (parseInstructions text)

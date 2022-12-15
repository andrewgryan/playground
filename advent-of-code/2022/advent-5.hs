import Text.Read
import Data.List (reverse, transpose)

data Crate = Crate Char deriving (Show)
data Stack = Stack Char [Crate] deriving (Show)
data Instruction = Instruction Int Int Int deriving (Show)

toStack :: String -> Stack
toStack s =
    case s of
        (x:xs) -> Stack x (((fmap Crate) . (filter notBlank)) xs)
        _ -> Stack ' ' []

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

main = do
    text <- readFile "input-5"
    print (parseBoard text)

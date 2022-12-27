import Control.Applicative
import qualified Parsing
import qualified Data.Maybe as Maybe

-- type Operation = String
-- type Item = Int
-- data Test = Test String String String
-- data Monkey = Monkey Int [Item] Operation Test


-- Monkey parser
data Monkey = Monkey
  { index :: Int
  , items :: [Int]
  , operation :: String
  , test :: String
  , ifTrue :: String
  , ifFalse :: String
  } deriving Show

startingItems :: Parsing.Parser [Int]
startingItems = do
  Parsing.string "Starting items:"
  Parsing.space
  integers

integers :: Parsing.Parser [Int]
integers = do
  x <- Parsing.integer
  xs <- many (do
      Parsing.space
      Parsing.char ','
      Parsing.space
      Parsing.integer)
  return (x:xs)
  
monkeyId :: Parsing.Parser Int
monkeyId = do
  Parsing.string "Monkey"
  Parsing.space
  n <- Parsing.nat
  Parsing.char ':'
  return n
  
newlineSeparated :: Parsing.Parser a ->  Parsing.Parser [a]
newlineSeparated p = do
  x <- p
  xs <- some (do
    some (Parsing.char '\n')
    p)
  return (x:xs)
  

monkey :: Parsing.Parser Monkey
monkey = do
  id <- monkeyId
  Parsing.newline
  Parsing.space
  items <- startingItems
  Parsing.newline
  operation' <- keyword "Operation:"
  Parsing.newline
  test' <- keyword "Test:"
  Parsing.newline
  ifTrue' <- keyword "If true:"
  Parsing.newline
  ifFalse' <- keyword "If false:"
  Parsing.newline
  return Monkey
    { index = id
    , items = items
    , operation = operation'
    , test = test'
    , ifTrue = ifTrue'
    , ifFalse = ifFalse'
    }
    

keyword :: String -> Parsing.Parser String
keyword key = do
  Parsing.space
  Parsing.string key
  Parsing.space
  chars
  
chars :: Parsing.Parser String
chars =
  many (Parsing.satisfy notEOL)
  
notEOL :: Char -> Bool
notEOL =
  (/= '\n')

-- Puzzle
example :: String
example = unlines
  [ "Monkey 0:"
  , "  Starting items: 79, 98"
  , "  Operation: new = old * 19"
  , "  Test: divisible by 23"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 1:"
  , "  Starting items: 54, 65, 75, 74"
  , "  Operation: new = old + 6"
  , "  Test: divisible by 19"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 0"
  , ""
  ]

main :: IO ()
main = do
  text <- readFile "input-11"
  let (monkeys, left) = Maybe.fromJust (Parsing.parse (newlineSeparated monkey) text)
  print monkeys

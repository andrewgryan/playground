import Control.Applicative
import qualified Parsing
import Parsing (string, nat, newline, space)
import qualified Data.Maybe as Maybe


-- Monkey parser
newtype Throw = ToMonkey Int deriving Show
newtype Test = DivisibleBy Int deriving Show

data Monkey = Monkey
  { index :: Int
  , items :: [Int]
  , operation :: Expression
  , test :: Test
  , ifTrue :: Throw
  , ifFalse :: Throw
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
  newline
  space
  items <- startingItems
  newline
  space
  operation' <- parseOperation
  space
  newline
  space
  string "Test:"
  space
  test' <- parseTest
  newline
  space
  string "If true:"
  space
  ifTrue' <- throw
  newline
  space
  string "If false:"
  space
  ifFalse' <- throw
  newline
  return Monkey
    { index = id
    , items = items
    , operation = operation'
    , test = test'
    , ifTrue = ifTrue'
    , ifFalse = ifFalse'
    }

parseTest :: Parsing.Parser Test
parseTest = do
  string "divisible by"
  space
  DivisibleBy <$> nat

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
  
-- Parse operation
-- old + 9
data Op = Add | Multiply deriving Show
data Term = Old | Value Int deriving Show
data Expression = Binary Op Term Term deriving Show

throw :: Parsing.Parser Throw
throw = do
  string "throw to monkey"
  space
  ToMonkey <$> nat

parseOperation :: Parsing.Parser Expression
parseOperation = do
  Parsing.string "Operation: new ="
  Parsing.space
  parseExpression

parseExpression :: Parsing.Parser Expression
parseExpression = do
  left <- parseTerm
  Parsing.space
  op <- parseOp
  Parsing.space
  right <- parseTerm
  return (Binary op left right)

parseOp :: Parsing.Parser Op
parseOp = do
  Parsing.char '+'
  return Add
  <|> do
  Parsing.char '*'
  return Multiply
  
parseTerm :: Parsing.Parser Term
parseTerm = do
  Parsing.string "old"
  return Old
  <|> do
  Value <$> nat
  
eval :: Expression -> Int -> Int
eval (Binary op lhs rhs) n =
  let
    left = deref lhs n
    right = deref rhs n
  in
  case op of
    Add ->
      left + right
    Multiply ->
      left * right

deref :: Term -> Int -> Int
deref Old n =
  n
deref (Value m) _ =
  m
  
-- Keep away
keepAway :: [Monkey] -> [Monkey]
keepAway monkeys =
  monkeys

inspectItem :: Monkey -> Int -> Int
inspectItem monkey =
  eval (operation monkey)
  
relief :: Int -> Int
relief = (`div` 3)

main :: IO ()
main = do
  text <- readFile "input-11"
  let (monkeys, left) = Maybe.fromJust (Parsing.parse (newlineSeparated monkey) text)
  -- print (fmap operation monkeys)
  print (head (items (head monkeys)))

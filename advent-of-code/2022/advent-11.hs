import Control.Applicative

-- type Operation = String
-- type Item = Int
-- data Test = Test String String String
-- data Monkey = Monkey Int [Item] Operation Test


-- Hand-rolled parsing library

newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\input ->
    case parse p input of
      Just (v, out) -> Just (g v, out)
      Nothing -> Nothing
    )
    
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\input -> Just (v, input))
  
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\input ->
    case parse pg input of
      Nothing -> Nothing
      Just (g, out) -> parse (fmap g px) out)
      
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input ->
    case parse p input of
      Nothing -> Nothing
      Just (v, out) -> parse (f v) out)

-- Choices
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const Nothing)
  
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\input -> case parse p input of
    Nothing -> parse q input
    Just (v, out) -> Just (v, out)) 

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

item :: Parser Char
item = P (\input -> case input of
  [] -> Nothing
  (x:xs) -> Just (x, xs))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  x <- item
  if p x then
    return x
  else
    empty

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

integer :: Parser Int
integer = do
  char '-'
  n <- nat
  return (-n)
  <|> nat

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

digit :: Parser Char
digit = satisfy isDigit

isDigit :: Char -> Bool
isDigit c =
    case c of
      '0' -> True
      '1' -> True
      '2' -> True
      '3' -> True
      '4' -> True
      '5' -> True
      '6' -> True
      '7' -> True
      '8' -> True
      '9' -> True
      _ -> False

char :: Char -> Parser Char
char c = satisfy (== c)

-- Handling space
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

space :: Parser ()
space = do
  many (satisfy isSpace)
  return ()

isSpace :: Char -> Bool
isSpace = (== ' ')

-- Monkey parser

newtype Monkey = Monkey Int deriving Show

monkey :: Parser Monkey
monkey = do
  string "Monkey"
  space
  Monkey <$> nat

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
  putStrLn example

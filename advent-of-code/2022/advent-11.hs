
type Operation = String
type Item = Int
data Test = Test String String String
data Monkey = Monkey Int [Item] Operation Test


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

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

-- nat :: Parser Int
-- nat = do
--   xs <- some digit
--   return (read xs)

integer :: Parser Int
integer = P (\s ->
  case parse (some digit) s of
    Just (d, s') -> Just (read d :: Int, s')
    _ -> Nothing)

some :: Parser a -> Parser [a]
some p = P (\s ->
  case parse p s of
    Just (v, s') ->
      case parse (some p) s' of
         Just (vs, r) -> Just (v:vs, r)
         _ -> Just ([v], s')
    _ -> Nothing)
    
digit :: Parser Char
digit = P digit'

digit' :: String -> Maybe (Char, String)
digit' s =
  case s of
    [] -> Nothing
    (c:cs) ->
      case c of
        '0' -> Just (c, cs)
        '1' -> Just (c, cs)
        '2' -> Just (c, cs)
        '3' -> Just (c, cs)
        '4' -> Just (c, cs)
        '5' -> Just (c, cs)
        '6' -> Just (c, cs)
        '7' -> Just (c, cs)
        '8' -> Just (c, cs)
        '9' -> Just (c, cs)
        _ -> Nothing

char :: Char -> Parser Char
char c = P (char' c)

char' :: Char -> String -> Maybe (Char, String)
char' c s =
    case s of
      [] -> Nothing
      (x:xs) -> if x == c then Just (x, xs) else Nothing


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

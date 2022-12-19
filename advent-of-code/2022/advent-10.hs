import Data.List.Split (splitOn)
import qualified Data.Maybe as Maybe

type Cycle = Int
type Register = Int
data CPU = CPU Cycle Register

boot :: CPU
boot =
  CPU 0 1
  
execute :: [Op] -> CPU -> CPU
execute ops cpu =
  cpu

data Op
  = Noop
  | Addx Int
  deriving Show

toOp :: String -> Maybe Op
toOp s =
  case splitOn " " s of
    ["noop"] -> Just Noop
    ["addx", x] -> Just (Addx (read x :: Int))
    _ -> Nothing

smallProgram :: [String]
smallProgram =
  [ "noop"
  , "addx 3"
  , "addx -5"
  ]

main = do
  print (Maybe.mapMaybe toOp smallProgram)

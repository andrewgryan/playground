import qualified Data.Maybe as Maybe
import Data.List.Split (splitOn)


data Line
    = Command Command
    | FileType FileType
    deriving Show
        
data Command
    = CD String
    | LS
    deriving Show
        
data FileType
    = Dir String
    | File Int String
    deriving (Show, Eq)

parseLine :: String -> Maybe Line
parseLine s =
    let
        parts = splitOn " " s
    in
    case parts of
        ("$":xs) -> Just (Command (Maybe.fromJust (toCommand xs)))
        _ -> Just (FileType (Maybe.fromJust (toFileType parts)))

toCommand :: [String] -> Maybe Command
toCommand parts =
    case parts of
        ("cd":dir:_) -> Just (CD dir)
        ("ls":_) -> Just LS
        _ -> Nothing

toFileType :: [String] -> Maybe FileType
toFileType parts =
    case parts of
        ("dir":dir:_) -> Just (Dir dir)
        (size:name:_) -> Just (File (read size :: Int) name)
        _ -> Nothing
                
data FileSystem = Nil | Node FileType FileSystem FileSystem deriving Show
data Terminal = Terminal [String] FileSystem deriving Show


newTerminal :: Terminal
newTerminal =
    Terminal [] Nil

process :: Terminal -> Line -> Terminal
process term line =
    case line of
        Command cmd ->
            processCmd cmd term
        FileType ft ->
            processFt ft term

processCmd :: Command -> Terminal -> Terminal
processCmd cmd (Terminal path fs) =
    case cmd of
        CD d ->
            let
               newPath = cd d path
            in
            Terminal newPath (mkdir newPath fs)
        _ -> Terminal path fs
                
mkdir :: [String] -> FileSystem -> FileSystem
mkdir path fs =
    case reverse path of
        [] -> fs
        (x:xs) ->
            case fs of
                Nil -> mkdir xs (Node (Dir x) Nil Nil)
                Node d left right ->
                    if d == Dir x then
                        mkdir xs right
                    else
                        mkdir xs left

cd :: String -> [String] -> [String]
cd subdir path =
   case subdir of
       ".." -> tail path
       _ -> subdir : path 

processFt :: FileType -> Terminal -> Terminal
processFt ft term =
    term

example :: [String]
example = [ "$ cd /"
          , "$ ls"
          , "dir a"
          , "14848514 b.txt"
          , "8504156 c.dat"
          , "dir d"
          , "$ cd a"
          , "$ ls"
          , "dir e"
          , "29116 f"
          , "2557 g"
          , "62596 h.lst"
          , "$ cd e"
          , "$ ls"
          , "584 i"
          , "$ cd .."
          , "$ cd .."
          , "$ cd d"
          , "$ ls"
          , "4060174 j"
          , "8033020 d.log"
          , "5626152 d.ext"
          , "7214296 k"
          ]


main = do
    let commands = Maybe.mapMaybe parseLine example
    print (foldl process newTerminal commands)
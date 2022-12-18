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
    | Listing Int String
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
        (size:name:_) -> Just (Listing (read size :: Int) name)
        _ -> Nothing

type Size = Int
type Name = String

data FileSystem
    = Directory Name [FileSystem]
    | File Size Name
    deriving Show

blank :: FileSystem
blank =
    Directory "root" []

exampleSystem =
    Directory "root"
        [ Directory "/"
            [ Directory "a"
                [ Directory "e"
                    [ File 584 "i"
                    ]
                , File 29116 "f"
                , File 2557 "g"
                , File 62596 "h.lst"
                ]
            , File 14848514 "b.txt"
            , File 8504156 "c.dat"
            , Directory "d"
                [ File 4060174 "j"
                , File 8033020 "d.log"
                , File 5626152 "d.ext"
                , File 7214296 "k"
                ]
            ]
        ]

data Crumb = Crumb Name [FileSystem] [FileSystem] deriving Show
type Session = (FileSystem, [Crumb])

cd :: String -> Session -> Maybe Session
cd ".." (fs, Crumb name lhs rhs:bs) =
    Just (Directory name (lhs ++ [fs] ++ rhs), bs)

cd name (fs, bs) =
    case fs of
        (Directory parentName items) ->
            let
                (ls, item:rs) = break (hasName name) items
                crumb = Crumb parentName ls rs
            in
            Just (item, crumb:bs)
        (File _ _) ->
            Nothing

hasName :: String -> FileSystem -> Bool
hasName name (File _ s) =
    name == s
hasName name (Directory s _) =
    name == s

mkdir :: String -> Session -> Maybe Session
mkdir dirName (fs, bs) =
    let
        dir = Directory dirName []
    in
    case fs of
        (Directory parentName items) ->
            Just (Directory parentName (dir:items), bs)
        (File _ _) ->
            Nothing

toFileSystem :: Session -> FileSystem
toFileSystem (fs, _) =
    fs

blankSession :: Session
blankSession =
    (blank, [])

interpret :: [Line] -> FileSystem
interpret lines =
    toFileSystem (foldl interpretLine blankSession lines)
        
interpretLine :: Session -> Line -> Session
interpretLine session line =
    case line of
        Command (CD s) -> session
        Command LS -> session
        FileType _ -> session


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
    mapM_ print commands
    print (interpret commands)

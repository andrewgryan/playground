import qualified Data.Set as Set

partOne :: String -> Int
partOne s =
   let
       width = 4
   in
   search width s 0 + width
    
partTwo :: String -> Int
partTwo s =
   let
       width = 14
   in
   search width s 0 + width
    
search :: Int -> String -> Int -> Int
search w s i =
    let
        chars = take w s
    in
    if length (Set.fromList chars) == w then
        i
    else
        search w (tail s) (i + 1)

main = do
    text <- readFile "input-6"
    print (partOne "bvwbjplbgvbhsrlpgdmjqwftvncz")
    print (partOne "nppdvjthqldpwncqszvftbrmjlhg")
    print (partOne "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
    print (partOne "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
    print (partOne text)
    print (partTwo "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
    print (partTwo text)

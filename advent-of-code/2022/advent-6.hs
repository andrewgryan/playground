import qualified Data.Set as Set

partOne :: String -> Int
partOne s =
   search s 0 + 4
    
search :: String -> Int -> Int
search s i =
    let
        chars = take 4 s
    in
    if length (Set.fromList chars) == 4 then
        i
    else
        search (tail s) (i + 1)

main = do
    text <- readFile "input-6"
    print (partOne "bvwbjplbgvbhsrlpgdmjqwftvncz")
    print (partOne "nppdvjthqldpwncqszvftbrmjlhg")
    print (partOne "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
    print (partOne "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
    print (partOne text)

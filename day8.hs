import Data.List
import Data.Ord

parse :: String -> [String]
parse "\n" = []
parse s = take layer s : parse (drop layer s)
  where layer = 25 * 6

part1 :: String -> Int
part1 input =
  let layer = minimumBy (comparing $ count '0') $ parse input
  in count '1' layer * count '2' layer
    where count c = length . filter (== c)

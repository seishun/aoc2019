import Data.List
import Data.Ord

width :: Int
width = 25

height :: Int
height = 6

parse :: String -> [String]
parse "\n" = []
parse s = take layer s : parse (drop layer s)
  where layer = width * height

draw :: String -> [String]
draw "" = []
draw i = (map color $ take width i) : draw (drop width i)
  where color '0' = ' '
        color '1' = '#'

part1 :: String -> Int
part1 input =
  let layer = minimumBy (comparing $ count '0') $ parse input
  in count '1' layer * count '2' layer
    where count c = length . filter (== c)

part2 :: String -> String
part2 = intercalate "\n" . draw . map (foldl1 stack) . transpose . parse
  where stack '2' p = p
        stack p _ = p

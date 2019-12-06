import Data.Map (Map)
import qualified Data.Map as Map

type Vector = (Int, Int)

parse :: String -> [(Vector, Int)]
parse "" = []
parse (',':xs) = parse xs
parse (d:xs) =
  let [(c,xs')] = reads xs
  in (case d of
    'U' -> (-1, 0)
    'L' -> ( 0,-1)
    'D' -> ( 1, 0)
    'R' -> ( 0, 1), c) : parse xs'

trace :: [(Vector, Int)] -> Map Vector Int
trace = fst . foldl add (Map.empty, ((0, 0), 0))
  where add (m, (p, s)) ((x', y'), c) =
          let points = take c $ drop 1 $ iterate (\(x, y) -> (x + x', y + y')) p
          in (Map.union m $ Map.fromList (zip points [s+1..]), (last points, s + c))

distance :: Vector -> Vector -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

part1 :: String -> Int
part1 input =
  let [first, second] = map (trace . parse) $ lines input
  in minimum $ map (distance (0,0)) $ Map.keys $ Map.intersection first second

part2 :: String -> Int
part2 input =
  let [first, second] = map (trace . parse) $ lines input
  in minimum $ map snd $ Map.toList $ Map.intersectionWith (+) first second

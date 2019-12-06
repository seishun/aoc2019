import Data.Set (Set)
import qualified Data.Set as Set 

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

trace :: [(Vector, Int)] -> Set Vector
trace = fst . foldl add (Set.empty, (0, 0))
  where add (s, p) ((x', y'), c) =
          let points = take c $ drop 1 $ iterate (\(x, y) -> (x + x', y + y')) p
          in (Set.union s $ Set.fromList points, last points)

distance :: Vector -> Vector -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

part1 :: String -> Int
part1 input =
  let [first, second] = map (trace . parse) $ lines input
  in minimum $ map (distance (0,0)) $ Set.toList $ Set.intersection first second

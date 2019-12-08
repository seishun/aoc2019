import Data.Map (Map)
import qualified Data.Map as Map

parse :: String -> (String, String)
parse [a1, a2, a3, ')', b1, b2, b3] = ([b1, b2, b3], [a1, a2, a3])

part1 :: String -> Int
part1 input =
  let (a, b) = unzip $ map parse $ lines input
      m = Map.fromList $ ("COM", 0) : zip a (map (succ . (Map.!) m) b)
  in Map.foldl' (+) 0 m

part2 :: String -> Int
part2 input =
  let (a, b) = unzip $ map parse $ lines input
      m = Map.fromList $ ("COM", []) : zip a (map ((:) <*> (Map.!) m) b)
      [you, san] = map (reverse . (Map.!) m) ["YOU", "SAN"]
  in length you + length san - 2 * (length $ takeWhile (uncurry (==)) $ zip you san)

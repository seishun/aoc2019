import Data.List
import qualified Data.Set as Set

angle :: (Int, Int) -> (Int, Int) -> (Int, Int)
angle (x1, y1) (x2, y2) =
  let x = x2 - x1
      y = y2 - y1
  in (x `div` gcd x y, y `div` gcd x y)

parse :: String -> [(Int, Int)]
parse = concat . map (uncurry line) . zip [0..] . lines
  where line x = map ((,) x) . elemIndices '#'

part1 :: String -> Int
part1 input = maximum $ map (Set.size . Set.fromList . angles) asteroids
  where asteroids = parse input
        angles asteroid = map (angle asteroid) $ filter (/= asteroid) asteroids

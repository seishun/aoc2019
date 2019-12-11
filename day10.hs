import Data.List
import Data.Ord
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

relative :: (Int, Int) -> (Int, Int) -> (Int, Int)
relative (x1, y1) (x2, y2) = (x2 - x1, y1 - y2)

angle :: (Int, Int) -> (Int, Int) -> (Int, Int)
angle a b =
  let (x, y) = relative a b
  in (x `div` gcd x y, y `div` gcd x y)

parse :: String -> [(Int, Int)]
parse = concat . map (uncurry line) . zip [0..] . lines
  where line y = map (flip (,) y) . elemIndices '#'

best :: [(Int, Int)] -> (Int, (Int, Int))
best asteroids =
  let visible = map (Set.size . Set.fromList . angles) asteroids
  in maximumBy (comparing fst) $ zip visible asteroids
  where angles asteroid = map (angle asteroid) $ filter (/= asteroid) asteroids

part1 :: String -> Int
part1 input = fst $ best asteroids
  where asteroids = parse input

part2 :: String -> Int
part2 input =
  let (x, y) = snd $ best asteroids
      (x', y') = (!! 199) $ Map.elems $ Map.fromList $ do
        asteroid <- asteroids
        guard $ asteroid /= (x, y)
        let (x', y') = relative (x, y) asteroid
        return (atan2 (-fromIntegral x') (-fromIntegral y'), (x', y'))
      in (x + x') * 100 + (y - y')
  where asteroids = parse input

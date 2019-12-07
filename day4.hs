import Control.Monad
import Data.List

passwords :: (Int -> Bool) -> (Int, Int) -> [String]
passwords crit (from, to) = do
  password <- show <$> [from..to]
  guard $ any (crit . length) $ group password
  guard $ all (uncurry (<=)) $ zip <*> tail $ password
  return password

part1 :: (Int, Int) -> Int
part1 = length . passwords (>= 2)

part2 :: (Int, Int) -> Int
part2 = length . passwords (== 2)

import Control.Monad

passwords :: (Int, Int) -> [Int]
passwords (from, to) = do
  password <- [from..to]
  let pairs = zip <*> tail $ show password
  guard $ any (uncurry (==)) pairs
  guard $ all (uncurry (<=)) pairs
  return password

part1 :: (Int, Int) -> Int
part1 = length . passwords

import Data.List
import Data.Map (Map(..))
import qualified Data.Map as Map

parse :: String ->  [Int]
parse ('<':'x':'=':xs) =
  let [(x, ',':' ':'y':'=':ys)] = reads xs
      [(y, ',':' ':'z':'=':zs)] = reads ys
      [(z, ">")] = reads zs
  in [x, y, z]

energy :: ([Int], [Int]) -> Int
energy (v, p) =
  let potential = sum $ map abs v
      kinetic = sum $ map abs p
  in potential * kinetic

step :: [(Int, Int)] -> [(Int, Int)]
step moons = do
  moon@(v, p) <- moons
  let v' = foldr ((+) . signum . subtract p) v $ map snd $ filter (/= moon) moons
  let p' = p + v'
  return (v', p')

circle :: [(Int, Int)] -> [(Int, Int)] -> Int
circle moons first =
  let moons' = step moons
  in if moons' == first then 1
  else succ $ circle moons' first

part1 :: String -> Int
part1 input =
  let axes = map (map ((,) 0)) $ transpose $ map parse $ lines input
  in sum $ map (energy . unzip) $ transpose $ map ((!! 1000) . iterate step) $ axes

part2 :: String -> Int
part2 input =
  let axes = map (map ((,) 0)) $ transpose $ map parse $ lines input
  in foldr lcm 1 $ map (\axis -> circle axis axis) axes

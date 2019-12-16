import Data.Char
import Data.List

parse :: String -> [Int]
parse = map digitToInt . head . lines

pattern :: Int -> [Int]
pattern i = tail $ cycle $ concatMap (replicate i) [0, 1, 0, -1]

phase :: [Int] -> [Int]
phase input = take (length input) $ map element patterns
  where patterns = map pattern [1..]
        element = (`mod` 10) . abs . sum . zipWith (*) input

part1 :: String -> String
part1 = map intToDigit . take 8 . (!! 100) . iterate phase . parse

part2 :: String -> String
part2 input =
  let offset = read $ take 7 input
      signal = concat $ replicate 10000 $ parse input
      slice = drop offset signal
  in map intToDigit $ take 8 $ iterate hack slice !! 100
  where hack = map ((`mod` 10) . abs) . scanr1 (+)

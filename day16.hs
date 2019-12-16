import Data.Char

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

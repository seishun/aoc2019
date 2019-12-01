fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

part1 :: String -> Int
part1 = sum . map fuel . map read . lines

part2 :: String -> Int
part2 = sum . map (sum . takeWhile (> 0) . drop 1 . iterate fuel) . map read . lines

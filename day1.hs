fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

part1 :: String -> Int
part1 = sum . map fuel . map read . lines

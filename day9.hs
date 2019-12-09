import Data.Map (Map(..))
import qualified Data.Map as Map

parse :: String -> Map Int Int
parse input = Map.fromList $ zip [0..] $ read $ "[" ++ input ++ "]"

run :: [Int] -> Int -> Int -> Map Int Int -> [Int]
run input pos base program =
  let op = Map.findWithDefault 0 pos program
      op' = op `mod` 100
      params = map resolve [0..]
        where resolve i =
                let mode = op `div` 10 ^ (2 + i) `mod` 10
                in case mode of
                  0 -> get (pos + i + 1)
                  1 -> pos + i + 1
                  2 -> base + get (pos + i + 1)
  in if op' == 99 then []
  else if op' == 3 then
    let (value:xs) = input
        program' = set (params !! 0) value
    in run xs (pos + 2) base program'
  else if op' == 4 then get (params !! 0) : run input (pos + 2) base program
  else if op' == 9 then run input (pos + 2) (base + get (params !! 0)) program
  else if op' > 2 && op' < 7 then
    let ip = case op' of
          5 -> if get (params !! 0) /= 0 then get (params !! 1) else pos + 3
          6 -> if get (params !! 0) == 0 then get (params !! 1) else pos + 3
    in run input ip base program
  else
    let result = case op' of
          1 -> get (params !! 0) + get (params !! 1)
          2 -> get (params !! 0) * get (params !! 1)
          7 -> if get (params !! 0) < get (params !! 1) then 1 else 0
          8 -> if get (params !! 0) == get (params !! 1) then 1 else 0
        program' = set (params !! 2) result
    in run input (pos + 4) base program'
  where get pos = Map.findWithDefault 0 pos program
        set pos value = Map.insert pos value program

part1 :: String -> Int
part1 = head . run [1] 0 0 . parse

part2 :: String -> Int
part2 = head . run [2] 0 0 . parse

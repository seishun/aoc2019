import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

parse :: String -> Seq Int
parse input = Seq.fromList $ read $ "[" ++ input ++ "]"

run :: [Int] -> Int -> Seq Int -> [Int]
run input pos program =
  let op :<| params = Seq.drop pos program
      op' = op `mod` 100
      params' = Seq.mapWithIndex resolve params
        where resolve i param =
                let mode = op `div` 10 ^ (2 + i) `mod` 10
                in if mode == 0 then Seq.index program param else param
  in if op' == 99 then []
  else if op' == 3 then
    let (value:xs) = input
        program' = Seq.update (Seq.index params 0) value program
    in run xs (pos + 2) program'
  else if op' == 4 then Seq.index params' 0 : run input (pos + 2) program
  else if op' > 2 && op' < 7 then
    let ip = case op' of
          5 -> if Seq.index params' 0 /= 0 then Seq.index params' 1 else pos + 3
          6 -> if Seq.index params' 0 == 0 then Seq.index params' 1 else pos + 3
    in run input ip program
  else
    let result = case op' of
          1 -> Seq.index params' 0 + Seq.index params' 1
          2 -> Seq.index params' 0 * Seq.index params' 1
          7 -> if Seq.index params' 0 < Seq.index params' 1 then 1 else 0
          8 -> if Seq.index params' 0 == Seq.index params' 1 then 1 else 0
        program' = Seq.update (Seq.index params 2) result program
    in run input (pos + 4) program'

part1 :: String -> Int
part1 = last . run [1] 0 . parse

part2 :: String -> Int
part2 = head . run [5] 0 . parse

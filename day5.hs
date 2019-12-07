import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

parse :: String -> Seq Int
parse input = Seq.fromList $ read $ "[" ++ input ++ "]"

run :: [Int] -> Int -> Seq Int -> [Int]
run input pos program = case Seq.drop pos program of
  99 :<| _ -> []
  3 :<| a :<| _ ->
    let (value:xs) = input
        program' = Seq.update a value program
    in run xs (pos + 2) program'
  op :<| params ->
    let op' = op `mod` 10
    in if op' == 4 then
      let a :<| _ = params
          a' = if op `div` 100 `mod` 100 == 0 then Seq.index program a else a
      in a' : run input (pos + 2) program
    else
      let a :<| b :<| c :<| _ = params
          a' = if op `div` 100 `mod` 10 == 0 then Seq.index program a else a
          b' = if op `div` 1000 `mod` 10 == 0 then Seq.index program b else b
          c' = case op' of
            1 -> a' + b'
            2 -> a' * b'
          program' = Seq.update c c' program
      in run input (pos + 4) program'

part1 :: String -> Int
part1 = last . run [1] 0 . parse

import Data.List
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

parse :: String -> Seq Int
parse input = Seq.fromList $ read $ "[" ++ input ++ "]"

run :: Int -> Seq Int -> [Int] -> ([Int], Maybe (Int, Seq Int))
run pos program input =
  let op :<| params = Seq.drop pos program
      op' = op `mod` 100
      params' = Seq.mapWithIndex resolve params
        where resolve i param =
                let mode = op `div` 10 ^ (2 + i) `mod` 10
                in if mode == 0 then Seq.index program param else param
  in if op' == 99 then ([], Nothing)
  else if op' == 3 then
    case input of
      (value:xs) ->
        let program' = Seq.update (Seq.index params 0) value program
        in run (pos + 2) program' xs
      [] -> ([], Just (pos, program))
  else if op' == 4 then
    let (output, ip) = run (pos + 2) program input
    in (Seq.index params' 0 : output, ip)
  else if op' > 2 && op' < 7 then
    let ip = case op' of
          5 -> if Seq.index params' 0 /= 0 then Seq.index params' 1 else pos + 3
          6 -> if Seq.index params' 0 == 0 then Seq.index params' 1 else pos + 3
    in run ip program input
  else
    let result = case op' of
          1 -> Seq.index params' 0 + Seq.index params' 1
          2 -> Seq.index params' 0 * Seq.index params' 1
          7 -> if Seq.index params' 0 < Seq.index params' 1 then 1 else 0
          8 -> if Seq.index params' 0 == Seq.index params' 1 then 1 else 0
        program' = Seq.update (Seq.index params 2) result program
    in run (pos + 4) program' input

loop :: Seq Int -> [Int] -> [Int]
loop program phases =
  let (output, states') = mapAccumL amp [0] phases
  in loop' states' output
  where amp input phase = run 0 program (phase : input)

loop' :: [Maybe (Int, Seq Int)] -> [Int] -> [Int]
loop' states input = case catMaybes states of
  [] -> input
  states ->
    let (output, states') = mapAccumL amp input states
    in loop' states' output
    where amp input (pos, program) = run pos program input

part1 :: String -> Int
part1 input = 
  let program = parse input
  in maximum [head $ loop program phases | phases <- permutations [0..4]]

part2 :: String -> Int
part2 input =
  let program = parse input
  in maximum [head $ loop program phases | phases <- permutations [5..9]]

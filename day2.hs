import Control.Monad
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

parse :: String -> Seq Int
parse input = Seq.fromList $ read $ "[" ++ input ++ "]"

run :: Int -> Seq Int -> Int
run pos program = case Seq.drop pos program of
  99 :<| _ -> Seq.index program 0
  op :<| a :<| b :<| c :<| _ ->
    let a' = Seq.index program a
        b' = Seq.index program b
        c' = case op of
          1 -> a' + b'
          2 -> a' * b'
        program' = Seq.update c c' program
    in run (pos + 4) program'

part1 :: String -> Int
part1 = run 0 . Seq.update 1 12 . Seq.update 2 2 . parse

part2 :: String -> Int
part2 = head . good . parse
  where good program = do
        noun <- [0..99]
        verb <- [0..99]
        let output = run 0 $ Seq.update 1 noun $ Seq.update 2 verb $ program
        guard $ output == 19690720
        return $ 100 * noun + verb

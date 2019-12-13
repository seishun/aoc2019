import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set

type IntcodeState = (Int, Int, Map Int Int)
data GameState = GameState { blocks :: Set (Int, Int)
                           , paddle :: Int
                           , ball :: Int
                           , score :: Int
                           }

parse :: String -> Map Int Int
parse input = Map.fromList $ zip [0..] $ read $ "[" ++ input ++ "]"

run :: IntcodeState -> [Int] -> ([Int], IntcodeState)
run (pos, base, program) input =
  let op = Map.findWithDefault 0 pos program
      op' = op `mod` 100
      params = map resolve [0..]
        where resolve i =
                let mode = op `div` 10 ^ (2 + i) `mod` 10
                in case mode of
                  0 -> get (pos + i + 1)
                  1 -> pos + i + 1
                  2 -> base + get (pos + i + 1)
  in if op' == 99 then ([], undefined)
  else if op' == 3 then
    case input of
      (value:xs) ->
        let program' = set (params !! 0) value
        in run ((pos + 2), base, program') xs
      [] -> ([], (pos, base, program))
  else if op' == 4 then
    let (output, ip) = run ((pos + 2), base, program) input
    in (get (params !! 0) : output, ip)
  else if op' == 9 then
    run ((pos + 2), (base + get (params !! 0)), program) input
  else if op' > 2 && op' < 7 then
    let ip = case op' of
          5 -> if get (params !! 0) /= 0 then get (params !! 1) else pos + 3
          6 -> if get (params !! 0) == 0 then get (params !! 1) else pos + 3
    in run (ip, base, program) input
  else
    let result = case op' of
          1 -> get (params !! 0) + get (params !! 1)
          2 -> get (params !! 0) * get (params !! 1)
          7 -> if get (params !! 0) < get (params !! 1) then 1 else 0
          8 -> if get (params !! 0) == get (params !! 1) then 1 else 0
        program' = set (params !! 2) result
    in run ((pos + 4), base, program') input
  where get pos = Map.findWithDefault 0 pos program
        set pos value = Map.insert pos value program

draw :: GameState -> [Int] -> GameState
draw gs [] = gs
draw gs (x:y:tile:xs) =
  let gs' = case (x, y, tile) of
        (-1, 0, score) -> gs { score = score }
        (x, y, 2) -> gs { blocks = Set.insert (x, y) $ blocks gs }
        (x, _, 3) -> gs { paddle = x }
        (x, _, 4) -> gs { ball = x }
        (x, y, _) -> gs { blocks = Set.delete (x, y) $ blocks gs }
  in draw gs' xs

play :: GameState -> IntcodeState -> Int
play gs@(GameState blocks paddle ball score) is =
  if Set.size blocks == 0 then score
  else let (output, is') = run is [signum (ball - paddle)]
  in play (draw gs output) is'

part1 :: String -> Int
part1 input =
  let program = parse input
  in Set.size $ blocks $ draw empty $ fst $ run (0, 0, program) []
  where empty = GameState Set.empty undefined undefined undefined

part2 :: String -> Int
part2 input =
  let program = parse input
      free2play = Map.insert 0 2 program
      (output, is) = run (0, 0, free2play) []
  in play (draw empty output) is
  where empty = GameState Set.empty undefined undefined undefined

import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set(..))
import qualified Data.Set as Set

type Location = (Int, Int)
type State = (Int, Int, Map Int Int)

parse :: String -> Map Int Int
parse input = Map.fromList $ zip [0..] $ read $ "[" ++ input ++ "]"

run :: State -> [Int] -> ([Int], State)
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
  in if op' == 99 then undefined
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

adjacent :: (Location, State) -> [(Bool, (Location, State))]
adjacent ((x, y), state) = do
  (move, pos') <- zip [1..4] [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
  let ([status], state') = run state [move]
  guard $ status /= 0
  return $ case status of
    1 -> (False, (pos', state'))
    2 -> (True, (pos', state'))

bfs1 :: Set Location -> Map Location State -> (Int, (Location, State))
bfs1 visited unvisited =
  let adjacent' = concatMap adjacent $ Map.toList unvisited
  in case filter fst adjacent' of
    [(_, ls)] -> (1, ls)
    [] ->
      let unvisited' = Map.withoutKeys (Map.fromList $ map snd adjacent') visited
          visited' = Set.union visited $ Map.keysSet unvisited
          (steps, ls) = bfs1 visited' unvisited'
      in (succ steps, ls)

bfs2 :: Set Location -> Map Location State -> Int
bfs2 visited unvisited =
  let adjacent' = concatMap adjacent $ Map.toList unvisited
      unvisited' = Map.withoutKeys (Map.fromList $ map snd adjacent') visited
      visited' = Set.union visited $ Map.keysSet unvisited
  in if Map.null unvisited' then 0
  else 1 + bfs2 visited' unvisited'

part1 :: String -> Int
part1 input =
  let program = parse input
  in fst $ bfs1 Set.empty $ Map.singleton (0, 0) (0, 0, program)

part2 :: String -> Int
part2 input =
  let program = parse input
      (pos, state) = snd $ bfs1 Set.empty $ Map.singleton (0, 0) (0, 0, program)
  in bfs2 Set.empty $ Map.singleton pos state

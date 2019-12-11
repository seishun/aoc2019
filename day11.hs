import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Ord

type State = (Int, Int, Map Int Int)

data Direction = U | R | D | L deriving (Enum)

turn :: Direction -> Int -> Direction
turn U 0 = L
turn L 1 = U
turn d 0 = pred d
turn d 1 = succ d

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) U = (x, pred y)
move (x, y) R = (succ x, y)
move (x, y) D = (x, succ y)
move (x, y) L = (pred x, y)

parse :: String -> Map Int Int
parse input = Map.fromList $ zip [0..] $ read $ "[" ++ input ++ "]"

run :: State -> [Int] -> ([Int], Maybe State)
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
  in if op' == 99 then ([], Nothing)
  else if op' == 3 then
    case input of
      (value:xs) ->
        let program' = set (params !! 0) value
        in run ((pos + 2), base, program') xs
      [] -> ([], Just (pos, base, program))
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

paint :: State -> (Int, Int) -> Direction -> Map (Int, Int) Int -> Int
paint state pos facing panels =
  let panel = Map.findWithDefault 0 pos panels
      ([color, direction], state') = run state [panel]
      panels' = Map.insert pos color panels
      facing' = turn facing direction
      pos' = move pos facing'
  in case state' of
    Nothing -> Map.size panels'
    Just state' -> paint state' pos' facing' panels'

part1 :: String -> Int
part1 input =
  let program = parse input
  in paint (0, 0, program) (0, 0) U Map.empty

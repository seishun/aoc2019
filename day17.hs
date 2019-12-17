import Control.Applicative
import Control.Monad
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Maybe

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

parameters :: Map (Int, Int) Char -> [Int]
parameters m = do
  ((x, y), point) <- Map.toList m
  guard $ point == '#'
  guard $ all scaffold [(pred x, y), (succ x, y), (x, pred y), (x, succ y)]
  return $ x * y
    where scaffold pos = Map.lookup pos m == Just '#'

left, right :: Char -> Char
left '^' = '<'
left '>' = '^'
left 'v' = '>'
left '<' = 'v'
right '^' = '>'
right '>' = 'v'
right 'v' = '<'
right '<' = '^'

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, pred y)
move (x, y) '>' = (succ x, y)
move (x, y) 'v' = (x, succ y)
move (x, y) '<' = (pred x, y)

data Move = TL | TR | MF Int deriving (Eq)
instance Show Move where
  show TL = "L"
  show TR = "R"
  show (MF units) = show units

view :: [String] -> Map (Int, Int) Char
view lines = Map.fromList $ do
  (y, line) <- zip [0..] lines
  (x, point) <- zip [0..] line
  return ((x, y), point)

path :: (Int, Int) -> Char -> Map (Int, Int) Char -> [Move]
path pos dir0 m = fromMaybe [] $
    go dir0 id <|>
    go (left dir0) (TL :) <|>
    go (right dir0) (TR :)
  where
    go dir f =
      let next = move pos dir
      in case Map.findWithDefault '.' next m of
        '#' -> Just (f $ MF 1 : path next dir m)
        '.' -> Nothing

pathify :: [Move] -> [Move]
pathify moves = map p $ group moves
  where p [TR] = TR
        p [TL] = TL
        p l = MF $ length l

collapse :: [Move] -> [[Move]] -> [([Int], [[Move]])]
collapse [] fs = [([], fs)]
collapse moves fs =
  let splits = zip (tail $ inits moves) (tail $ tails moves)
      matches = mapMaybe (\(i, t) -> ((,) t) <$> elemIndex i fs) splits
  in if null matches then
    if length fs == 3 then []
    else do
      (i, t) <- filter (\(i, t) -> isInfixOf i t) splits
      (is, fs') <- collapse t (fs ++ [i])
      return (length fs : is, fs')
  else do
      (t, i) <- matches
      (is, fs') <- collapse t fs
      return (i : is, fs')

part1 :: String -> Int
part1 = sum . parameters . view . lines . map toEnum . run [] 0 0 . parse

part2 :: String -> Int
part2 input =
  let program = parse input
      m = view $ lines $ map toEnum $ run [] 0 0 program
      (pos, dir) = head $ Map.toList $ Map.filter (`elem` "^>v<") m
      i = intercalate "\n" $ head $ do
        (i, fs) <- collapse (pathify $ path pos dir m) []
        let i' = intersperse ',' $ map ("ABC" !!) i
        guard $ length i' <= 20
        let fs' = map (intercalate "," . map show) fs
        guard $ all ((<= 20) . length) fs'
        return $ i' : fs'
  in last $ run (map fromEnum $ i ++ "\nn\n") 0 0 $ Map.insert 0 2 program

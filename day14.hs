import Data.Map (Map(..))
import qualified Data.Map as Map

type Reaction = (Int, String, Int, String)

parse' :: [String] -> ([Reaction], (Int, String))
parse' ["=>", ou, oc] = ([], (read ou, oc))
parse' (iu : ic : xs) =
  let (i, o@(ou, oc)) = parse' xs
  in ((read iu, ic, ou, oc) : i, o)

parse :: String -> [Reaction]
parse = concatMap (fst . parse' . words . filter (/= ',')) . lines

ore :: Int -> [Reaction] -> Int
ore fuel reactions =
  let m = Map.fromListWith (+) $ ("FUEL", fuel) : map require reactions
        where require (iu, ic, ou, oc) =
                let ceil = (m Map.! oc + ou - 1) `div` ou
                in (ic, iu * ceil)
  in m Map.! "ORE"

bs' :: Int -> Int -> [Reaction] -> Int
bs' lo hi reactions
  | succ lo == hi = lo
  | ore mid reactions > 1000000000000 = bs' lo mid reactions
  | otherwise = bs' mid hi reactions
  where mid = (lo + hi) `div` 2

bs :: Int -> [Reaction] -> Int
bs fuel reactions
  | ore (fuel * 2) reactions > 1000000000000 = bs' fuel (fuel * 2) reactions
  | otherwise = bs (fuel * 2) reactions

part1 :: String -> Int
part1 = ore 1 . parse

part2 :: String -> Int
part2 = bs 1 . parse

import Data.Map (Map(..))
import qualified Data.Map as Map

parse :: [String] -> ([(Int, String, Int, String)], (Int, String))
parse ["=>", ou, oc] = ([], (read ou, oc))
parse (iu : ic : xs) =
  let (i, o@(ou, oc)) = parse xs
  in ((read iu, ic, ou, oc) : i, o)

part1 :: String -> Int
part1 input =
  let reactions = map (fst . parse . words . filter (/= ',')) $ lines input
      m = Map.fromListWith (+) $ ("FUEL", 1) : map require (concat reactions)
        where require (iu, ic, ou, oc) =
                let ceil = (m Map.! oc + ou - 1) `div` ou
                in (ic, iu * ceil)
  in m Map.! "ORE"

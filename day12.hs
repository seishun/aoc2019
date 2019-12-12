import Control.Monad

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)

parse :: String ->  (Int, Int, Int)
parse ('<':'x':'=':xs) =
  let [(x, ',':' ':'y':'=':ys)] = reads xs
      [(y, ',':' ':'z':'=':zs)] = reads ys
      [(z, ">")] = reads zs
  in (x, y, z)

gravitate :: Position -> Velocity -> Position -> Velocity
gravitate (px, py, pz) (vx, vy, vz) (px', py', pz') =
  let vx' = vx + signum (px' - px)
      vy' = vy + signum (py' - py)
      vz' = vz + signum (pz' - pz)
  in (vx', vy', vz')

velocitate :: Position -> Velocity -> Position
velocitate (px, py, pz) (vx, vy, vz) = (px + vx, py + vy, pz + vz)

energy :: (Velocity, Position) -> Int
energy ((vx, vy, vz), (px, py, pz)) =
  let potential = abs px + abs py + abs pz
      kinetic = abs vx + abs vy + abs vz
  in potential * kinetic

step :: [(Velocity, Position)] -> [(Velocity, Position)]
step moons = do
  moon@(v, p) <- moons
  let v' = foldl (gravitate p) v $ map snd $ filter (/= moon) moons
  let p' = velocitate p v'
  return (v', p')

part1 :: String -> Int
part1 input =
  let moons = zip (repeat (0, 0, 0)) $ map parse $ lines input
  in sum $ map energy $ iterate step moons !! 1000

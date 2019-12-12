-- file src/Day10.hs
module Day10 ( parseInput
             , part1
             , part2
             ) where

import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (maximumBy, sortBy, transpose, (!!))
import Data.Ord (Ordering(..), comparing)
import Control.Lens.Combinators (ifoldMap)
import Linear (V2(..), (^-^), (^/))

type Coord = V2 Int
type AsteroidMap = [Coord]

parseInput :: String -> AsteroidMap
parseInput = ifoldMap (\y -> ifoldMap (\x -> parse (V2 y x))) . lines
  where parse _ '.' = []
        parse p _   = [p]

part1 :: AsteroidMap -> Int
part1 = length . snd . monitoringStation

part2 :: AsteroidMap -> Int
part2 asteroids = 100 * x + y
  where V2 y x = (zapOrder asteroids) !! 199

zapOrder :: AsteroidMap -> [Coord]
zapOrder asteroids = concat
                   . transpose
                   . map (sortBy . comparing $ distance station)
                   . map (buckets M.!)
                   $ sortBy clockwise sl
  where buckets = bySightline station asteroids
        (station, sl) = monitoringStation asteroids

        distance a b = fmap abs (b ^-^ a)

bySightline :: Coord -> AsteroidMap -> Map Coord [Coord]
bySightline from = foldr go M.empty
  where go to m
          | from == to = m
          | otherwise  = M.insertWith (++) (sightline from to) [to] m

monitoringStation :: AsteroidMap -> (Coord, [Coord])
monitoringStation m = maximumBy (comparing $ length . snd)
                    $ map (\c -> (c, sightlines c m)) m

sightlines :: Coord -> AsteroidMap -> [Coord]
sightlines from = S.toList . foldr go S.empty
  where go to s
          | from == to = s
          | otherwise  = sightline from to `S.insert` s

sightline :: Coord -> Coord -> Coord
sightline from to = reduce (to ^-^ from)
  where reduce v@(V2 y x) = fmap (`div` gcd x y) v

clockwise :: Coord -> Coord -> Ordering
clockwise (V2 ay ax) (V2 by bx)
   | ax >= 0 && bx < 0 = LT
   | ax < 0 && bx >= 0 = GT
   | cross < 0 = GT
   | cross > 0 = LT
   where cross = ax * by - bx * ay

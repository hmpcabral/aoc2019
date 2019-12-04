-- file src/Day03.hs
module Day03 ( parseInput
             , part1
             , part2
             ) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

data Direction = Direction Int Int

data Coord = Coord Int Int
           deriving (Eq, Ord)

type Wire = [Direction]

parseInput :: String -> [Wire]
parseInput = map parseWire . lines
  where parseWire = map parseDir . splitOn ","
        parseDir (x:xs)
          | x == 'R' = Direction n 0
          | x == 'L' = Direction (-n) 0
          | x == 'U' = Direction 0 n
          | x == 'D' = Direction 0 (-n)
          where n = read xs

part1 :: [Wire] -> Int
part1 [v, w] = minimum . map (distance origin) $ intersect v w

part2 :: [Wire] -> Int
part2 [v, w] = minimum . map combine $ intersect v w
  where combine p = (stepsTo v origin p 0) + (stepsTo w origin p 0)

origin :: Coord
origin = Coord 0 0

intersect :: Wire -> Wire -> [Coord]
intersect v w = S.toList $ S.intersection (walk v) (walk w)

distance :: Coord -> Coord -> Int
distance (Coord px py) (Coord qx qy) = abs (px - qx) + abs (py - qy)

move :: Coord -> Direction -> Coord
move (Coord x y) (Direction dx dy) = Coord (x + dx) (y + dy)

walk :: Wire -> Set Coord
walk w = walkFrom w (Coord 0 0) S.empty

walkFrom :: Wire -> Coord -> Set Coord -> Set Coord
walkFrom [] _ m = m
walkFrom (d:ds) p m = walkFrom ds (move p d) $ S.union m leg
  where leg = S.fromList $ steps p d

steps :: Coord -> Direction -> [Coord]
steps p (Direction dx dy) = [ move p (Direction dx' dy')
                            | dx' <- range dx
                            , dy' <- range dy ]
  where range d | d < 0  = [d..(-1)]
                | d > 0  = [1..d]
                | d == 0 = [0]

stepsTo :: Wire -> Coord -> Coord -> Int -> Int
stepsTo (d:ds) from to acc
  | goesThrough from d to = acc + distance from to
  | otherwise             = stepsTo ds (move from d) to (acc + delta d)
  where delta (Direction dx dy) = abs dx + abs dy

goesThrough :: Coord -> Direction -> Coord -> Bool
goesThrough p@(Coord px py) d (Coord qx qy)
  = between px px' qx && between py py' qy
  where Coord px' py' = move p d
        between a b x | a < b     = a <= x && x <= b
                      | otherwise = b <= x && x <= a

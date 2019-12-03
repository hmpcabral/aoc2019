module Day03 ( parseInput
             , part1
             , part2
             ) where

import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

data Direction = GoRight | GoLeft | GoUp | GoDown
               deriving (Show)

type Wire = [(Direction, Int)]
type Coord = (Int, Int)

parseInput :: String -> [Wire]
parseInput = map parseWire . lines
  where parseWire = map parseDir . splitOn ","
        parseDir (x:xs) = (dir x, read xs)
        dir x
          | x == 'R' = GoRight
          | x == 'L' = GoLeft
          | x == 'U' = GoUp
          | x == 'D' = GoDown

part1 :: [Wire] -> Int
part1 [v, w] = minimum . map dist . H.keys $ H.intersection (walk v) (walk w)
  where dist (x, y) = abs x + abs y

part2 :: [Wire] -> Int
part2 [v, w] = minimum . H.elems $ H.intersectionWith (+) (walk v) (walk w)

walk :: Wire -> HashMap Coord Int
walk w = walkFrom w (0, 0) 0 H.empty

walkFrom :: Wire -> Coord -> Int -> HashMap Coord Int -> HashMap Coord Int
walkFrom [] _ _ m = m
walkFrom ((d, n):ws) p s m = walkFrom ws (step p d n) (s+n) $ H.union m arm
  where arm = H.fromList [(step p d i, s+i) | i <- [1..n]]

step :: Coord -> Direction -> Int -> Coord
step (x, y) GoRight n = (x+n, y)
step (x, y) GoLeft  n = (x-n, y)
step (x, y) GoUp    n = (x, y+n)
step (x, y) GoDown  n = (x, y-n)

-- file src/Day04.hs
module Day04 ( parseInput
             , part1
             , part2
             ) where

import Data.List.Split (splitOn)
import Data.List (group)

parseInput :: String -> (Int, Int)
parseInput input = (low, high)
  where [low, high] = map read $ splitOn "-" input

part1 :: (Int, Int) -> Int
part1 = countValid [monotonic . show, doubles . show]

part2 :: (Int, Int) -> Int
part2 = countValid [monotonic . show, strictDoubles . show]

countValid :: [Int -> Bool] -> (Int, Int) -> Int
countValid fs = length . filter (\x -> all ($ x) fs) . range

range :: (Int, Int) -> [Int]
range (low, high) = [low..high]

monotonic :: Ord a => [a] -> Bool
monotonic xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

doubles :: Eq a => [a] -> Bool
doubles = any (\xs -> length xs >= 2) . group

strictDoubles :: Eq a => [a] -> Bool
strictDoubles = any (\xs -> length xs == 2) . group

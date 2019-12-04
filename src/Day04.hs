-- file src/Day04.hs
module Day04 ( parseInput
             , part1
             , part2
             ) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

parseInput :: String -> (Int, Int)
parseInput input = (low, high)
  where [low, high] = map read $ splitOn "-" input

part1 :: (Int, Int) -> Int
part1 (low, high) = S.size . S.filter match $ nDigit 6
  where match n = inRange low high n && hasDouble n

part2 :: (Int, Int) -> Int
part2 (low, high) = S.size . S.filter match $ nDigit 6
  where match n = inRange low high n && hasStrictDouble n

nDigit :: Int -> Set [Int]
nDigit n = S.fromList $ concat [aux [] dn | dn <- [1..9]]
  where aux ds d
          | length ds' == n = [ds']
          | otherwise       = [d..9] >>= aux ds'
          where ds' = d:ds

inRange :: Int -> Int -> [Int] -> Bool
inRange low high ds = low <= n && n <= high
  where n = sum $ zipWith (\d i -> d * 10^i) ds [0..]

hasDouble :: [Int] -> Bool
hasDouble (d:ds) = aux d ds
  where aux y (x:xs)
          | y == x    = True
          | otherwise = aux x xs
        aux _ []      = False

hasStrictDouble :: [Int] -> Bool
hasStrictDouble (d:ds) = aux 1 d ds
  where aux n y (x:xs)
          | y == x    = aux (n+1) x xs
          | n == 2    = True
          | otherwise = aux 1 x xs
        aux n _ []    = n == 2

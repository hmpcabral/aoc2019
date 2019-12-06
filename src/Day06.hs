-- file src/Day06.hs
module Day06 ( parseInput
             , part1
             , part2
             ) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

type OrbitalMap = Map String [String]

parseInput :: String -> OrbitalMap
parseInput s = M.fromListWith (++) adj
  where adj = [(x, [y]) | [x, y] <- map (splitOn ")") $ lines s]

part1 :: OrbitalMap -> Int
part1 m = sum . M.elems $ addOrbits m 0 "COM" M.empty

part2 :: OrbitalMap -> Int
part2 om = length me + length santa
  where Just xs = path om "YOU" "COM"
        Just ys = path om "SAN" "COM"
        (me, santa) = uniqueSuffix xs ys

orbiting :: String -> OrbitalMap -> [String]
orbiting = M.findWithDefault []

addOrbits :: OrbitalMap -> Int -> String -> Map String Int -> Map String Int
addOrbits om depth object cm = foldr go cm' $ orbiting object om
  where cm' = M.insert object depth cm
        go = addOrbits om (depth + 1)

uniqueSuffix :: Eq a => [a] -> [a] -> ([a], [a])
uniqueSuffix (x:xs) (y:ys)
  | x == y    = uniqueSuffix xs ys
  | otherwise = (x:xs, y:ys)

path :: OrbitalMap -> String -> String -> Maybe [String]
path om to from
  | from == to    = Just []
  | null branches = Nothing
  | otherwise     = fmap (from :) $ head branches
  where branches = filter isJust $ map (path om to) (orbiting from om)

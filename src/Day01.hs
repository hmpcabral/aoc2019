-- file src/Day01.hs
module Day01 ( parseInput
             , part1
             , part2
             ) where

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 = sum . map fuelModule

part2 :: [Int] -> Int
part2 = sum . map totalFuelModule

fuelModule :: Int -> Int
fuelModule m = m `div` 3 - 2

totalFuelModule :: Int -> Int
totalFuelModule = sum . takeWhile (> 0) . tail . iterate fuelModule

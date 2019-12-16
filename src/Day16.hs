-- file src/Day16.hs
module Day16 ( parseInput
             , part1
             , part2
             ) where

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.List ((!!))

parseInput :: String -> [Int]
parseInput = map digitToInt . filter isDigit

part1 :: [Int] -> String
part1 = display . take 8 . (!! 100) . iterate fft

part2 :: [Int] -> String
part2 input
  | 2 * offset < length input = error "No fast algorithm for short offsets!"
  | otherwise = display
              . take 8
              . (!! 100)
              . iterate (scanr1 reduce)
              . drop offset
              . concat
              $ replicate 10000 input
  where offset = sum $ zipWith (*) (map (10^) [6,5..0]) (take 7 input)
        reduce a b = (a + b) `mod` 10

fft :: [Int] -> [Int]
fft input = take (length input) $ map (\p -> reduce $ zipWith (*) p input) patterns
  where reduce = (`mod` 10) . abs . sum

patterns :: [[Int]]
patterns = map (tail . cycle . pattern [0, 1, 0, (-1)]) [1..]
  where pattern base n = concat $ foldr (\d ds -> replicate n d : ds) [] base

display :: [Int] -> String
display = map intToDigit

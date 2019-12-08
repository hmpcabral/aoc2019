-- file src/Day08.hs
module Day08 ( parseInput
             , part1
             , part2
             ) where

import Data.List.Split (chunksOf)
import Data.List (minimumBy)
import Data.Char (isDigit)
import Data.Ord (comparing)

type Image = [Layer]
type Layer = [Int]

width :: Int
width = 25

height :: Int
height = 6

parseInput :: String -> Image
parseInput s = chunksOf (width * height) ds
  where ds = map (read . (:[])) $ takeWhile isDigit s

part1 :: Image -> Int
part1 img = nDigits 1 layer * nDigits 2 layer
  where nDigits n = length . filter (== n)
        layer = minimumBy (comparing (nDigits 0)) img

part2 :: Image -> String
part2 = display width . foldr overlay base
  where base = replicate (width * height) 2

overlay :: Layer -> Layer -> Layer
overlay top bottom = map overlayPixel $ zip top bottom
  where overlayPixel (2, b) = b
        overlayPixel (t, _) = t

display :: Int -> Layer -> String
display w = unlines . map mkLine . chunksOf w
  where mkLine = map (\d -> if d == 1 then '*' else ' ')

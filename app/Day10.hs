-- file app/Day10.hs
module Main (main) where

import Day10

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

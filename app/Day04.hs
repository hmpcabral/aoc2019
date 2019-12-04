-- file app/Day04.hs
module Main (main) where

import Day04

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

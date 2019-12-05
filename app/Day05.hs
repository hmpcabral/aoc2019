-- file app/Day05.hs
module Main (main) where

import Day05

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

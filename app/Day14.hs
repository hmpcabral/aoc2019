-- file app/Day14.hs
module Main (main) where

import Day14

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

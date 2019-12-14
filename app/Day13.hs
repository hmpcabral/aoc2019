-- file app/Day13.hs
module Main (main) where

import Day13

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

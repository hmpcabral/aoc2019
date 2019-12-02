-- file app/Day02.hs
module Main (main) where

import Day02

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

-- file app/Day03.hs
module Main (main) where

import Day03

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

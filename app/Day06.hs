-- file app/Day06.hs
module Main (main) where

import Day06

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

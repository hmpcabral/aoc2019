-- file app/Day01.hs
module Main (main) where

import Day01Solution

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         print $ part2 parsed

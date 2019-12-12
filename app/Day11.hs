-- file app/Day11.hs
module Main (main) where

import Day11

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         putStrLn $ part2 parsed

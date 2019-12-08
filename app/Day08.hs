-- file app/Day08.hs
module Main (main) where

import Day08

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         print $ part1 parsed
         putStrLn $ part2 parsed

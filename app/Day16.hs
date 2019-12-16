-- file app/Day16.hs
module Main (main) where

import Day16

main :: IO ()
main = do
         input <- getContents
         let parsed = parseInput input
         putStrLn $ part1 parsed
         putStrLn $ part2 parsed

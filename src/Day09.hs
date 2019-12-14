-- file src/Day09.hs
{-# LANGUAGE RecordWildCards #-}

module Day09 ( parseInput
             , part1
             , part2
             ) where

import Computer

parseInput :: String -> Computer
parseInput = readProgram

part1 :: Computer -> Int
part1 = head . execute [1]

part2 :: Computer -> Int
part2 = head . execute [2]

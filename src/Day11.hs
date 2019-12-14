-- file src/Day11.hs
{-# LANGUAGE RecordWildCards #-}

module Day11 ( parseInput
             , part1
             , part2
             ) where

import Computer

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List.Split (chunksOf)
import Data.List (scanl')
import Linear (V2(..), (^+^), (!*))

type Coord = V2 Int

data Robot = Robot { position :: Coord
                   , heading :: V2 Int }

type Hull = Map Coord Int

parseInput :: String -> Computer
parseInput = readProgram

part1 :: Computer -> Int
part1 computer = M.size $ paint computer M.empty

part2 :: Computer -> String
part2 computer = display . paint computer $ M.singleton (V2 0 0) 1

display :: Hull -> String
display hull = unlines [[panel (V2 y x) | x <- [lx..rx]] | y <- [ty..by]]
  where (V2 ty lx, _) = M.findMin hull
        (V2 by rx, _) = M.findMax hull

        panel v = case M.findWithDefault 0 v hull of
                    0 -> ' '
                    _ -> '#'

paint :: Computer -> Hull -> Hull
paint computer hull = snd $ last evolution
  where evolution = scanl' paintPanel (robot, hull) $ chunksOf 2 instructions
        instructions = execute camera computer
        camera = map (\(r, h) -> M.findWithDefault 0 (position r) h) evolution
        robot = Robot (V2 0 0) (V2 (-1) 0)

paintPanel :: (Robot, Hull) -> [Int] -> (Robot, Hull)
paintPanel (Robot pos hdg, hull) [color, turn] = (Robot pos' hdg', hull')
  where hull' = M.insert pos color hull

        pos' = pos ^+^ hdg'
        hdg' = rotate turn hdg

        rotate 0 p = V2 (V2 0 (-1)) (V2 1 0) !* p
        rotate 1 p = V2 (V2 0 1) (V2 (-1) 0) !* p

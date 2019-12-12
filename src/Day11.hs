-- file src/Day11.hs
{-# LANGUAGE RecordWildCards #-}

module Day11 ( parseInput
             , part1
             , part2
             ) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn, chunksOf)
import Data.List (scanl')
import Linear (V2(..), (^+^), (!*))

data Computer = Computer { ip :: Int
                         , rom :: Vector Int
                         , ram :: IntMap Int
                         , relBase :: Int }

type Coord = V2 Int

data Robot = Robot { position :: Coord
                   , heading :: V2 Int }

type Hull = Map Coord Int

parseInput :: String -> Computer
parseInput s = Computer { ip = 0
                        , rom = V.fromList . map read $ splitOn "," s
                        , ram = IM.empty
                        , relBase = 0 }

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

store :: Int -> Int -> Computer -> Computer
store pos val c@Computer{..} = if pos < V.length rom
                                 then c { rom = rom V.// [(pos, val)] }
                                 else c { ram = IM.insert pos val ram }

load :: Int -> Computer -> Int
load pos Computer{..} = if pos < V.length rom
                            then rom V.! pos
                            else IM.findWithDefault 0 pos ram

execute :: [Int] -> Computer -> [Int]
execute input cur@Computer{..}
  = case opcode of
      -- add
      1  -> execute input $ binary (+) cur

      -- multiply
      2  -> execute input $ binary (*) cur

      -- input
      3  -> execute (tail input) . move (+2) $ store (output 1) (head input) cur

      -- output
      4 -> (operand 1 :) . execute input $ move (+2) cur

      -- jump if true
      5  -> execute input $ jumpIf (operand 1 /= 0) (const $ operand 2) (+3) cur

      -- jump if false
      6  -> execute input $ jumpIf (operand 1 == 0) (const $ operand 2) (+3) cur

      -- less than
      7  -> execute input $ binary (\x y -> if x < y then 1 else 0) cur

      -- equals
      8  -> execute input $ binary (\x y -> if x == y then 1 else 0) cur

      -- adjust relative base
      9  -> execute input . move (+2) $ cur { relBase = relBase + operand 1 }

      -- halt
      99 -> []

  where move f c = c { ip = f ip }
        binary op = move (+4) . store (output 3) (operand 1 `op` operand 2)
        jumpIf cond fTrue fFalse = move (if cond then fTrue else fFalse)

        output p = case mode p of
                      0 -> loadRel p
                      2 -> relBase + loadRel p

        operand p = case mode p of
                      0 -> load (loadRel p) cur
                      1 -> loadRel p
                      2 -> load (relBase + loadRel p) cur

        instr = loadRel 0
        opcode = instr `mod` 100
        mode p = instr `div` (10 ^ (p + 1)) `mod` 10

        loadRel d = load (ip + d) cur

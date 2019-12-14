-- file src/Computer.hs
{-# LANGUAGE RecordWildCards #-}

module Computer ( Computer(..)
                , readProgram
                , store
                , load
                , execute
                ) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List.Split (splitOn)

data Computer = Computer { ip :: Int
                         , rom :: Vector Int
                         , ram :: IntMap Int
                         , relBase :: Int }
              deriving (Show)

readProgram :: String -> Computer
readProgram s = Computer { ip = 0
                         , rom = V.fromList . map read $ splitOn "," s
                         , ram = IM.empty
                         , relBase = 0 }

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

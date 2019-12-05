-- file src/Day05.hs
module Day05 ( parseInput
             , part1
             , part2
             , execute
             ) where

import Data.Vector.Unboxed (Vector(..), fromList, (//), (!))
import Data.List.Split (splitOn)

type Program = Vector Int

parseInput :: String -> Program
parseInput = fromList . map read . splitOn ","

part1 :: Program -> Int
part1 = head . fst . execute [1]

part2 :: Program -> Int
part2 = head . fst . execute [5]

execute :: [Int] -> Program -> ([Int], Program)
execute = executeAt 0 []

executeAt :: Int -> [Int] -> [Int] -> Program -> ([Int], Program)
executeAt pos outp inp prog
  = case opcode of
      -- add
      1  -> executeAt (pos + 4) outp inp
              $ prog // [(load 3, operand 1 + operand 2)]

      -- multiply
      2  -> executeAt (pos + 4) outp inp
              $ prog // [(load 3, operand 1 * operand 2)]

      -- input
      3  -> executeAt (pos + 2) outp (tail inp) $ prog // [(load 1, head inp)]

      -- output
      4  -> executeAt (pos + 2) (operand 1 : outp) inp prog

      -- jump if true
      5  -> if operand 1 /= 0
              then executeAt (operand 2) outp inp prog
              else executeAt (pos + 3) outp inp prog

      -- jump if false
      6  -> if operand 1 == 0
              then executeAt (operand 2) outp inp prog
              else executeAt (pos + 3) outp inp prog

      -- less than
      7  -> executeAt (pos + 4) outp inp
              $ prog // [(load 3, if operand 1 < operand 2 then 1 else 0)]

      -- equals
      8  -> executeAt (pos + 4) outp inp
              $ prog // [(load 3, if operand 1 == operand 2 then 1 else 0)]

      -- halt
      99 -> (outp, prog)

  where load p = prog ! (pos + p)
        instr = load 0
        opcode = instr `mod` 100
        mode p = instr `div` (10 ^ (p + 1)) `mod` 10
        operand p = case mode p of
                      0 -> prog ! load p
                      1 -> load p

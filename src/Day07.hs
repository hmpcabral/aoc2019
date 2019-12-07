-- file src/Day07.hs
module Day07 ( parseInput
             , part1
             , part2
             ) where

import Data.Vector.Unboxed (Vector(..), fromList, (//), (!))
import Data.List.Split (splitOn)
import Data.List (permutations, last)

type Program = Vector Int

parseInput :: String -> Program
parseInput = fromList . map read . splitOn ","

part1 :: Program -> Int
part1 prog = maximum [series prog ps 0 | ps <- phases]
  where phases = permutations [0..4]

part2 :: Program -> Int
part2 prog = maximum [loop prog ps 0 | ps <- phases]
  where phases = permutations [5..9]

series :: Program -> [Int] -> Int -> Int
series _ [] signal = signal
series prog (p:ps) signal = series prog ps signal'
  where [signal'] = execute [p, signal] prog

loop :: Program -> [Int] -> Int -> Int
loop prog (p:ps) signal = last out
  where out = foldl (\i p -> run (p : i)) (run (p : signal : out)) ps
        run = (flip execute) prog

execute :: [Int] -> Program -> [Int]
execute = executeAt 0

executeAt :: Int -> [Int] -> Program -> [Int]
executeAt pos inp prog
  = case opcode of
      -- add
      1  -> executeAt (pos + 4) inp
              $ prog // [(load 3, operand 1 + operand 2)]

      -- multiply
      2  -> executeAt (pos + 4) inp
              $ prog // [(load 3, operand 1 * operand 2)]

      -- input
      3  -> executeAt (pos + 2) (tail inp) $ prog // [(load 1, head inp)]

      -- output
      4  -> operand 1 : executeAt (pos + 2) inp prog

      -- jump if true
      5  -> if operand 1 /= 0
              then executeAt (operand 2) inp prog
              else executeAt (pos + 3) inp prog

      -- jump if false
      6  -> if operand 1 == 0
              then executeAt (operand 2) inp prog
              else executeAt (pos + 3) inp prog

      -- less than
      7  -> executeAt (pos + 4) inp
              $ prog // [(load 3, if operand 1 < operand 2 then 1 else 0)]

      -- equals
      8  -> executeAt (pos + 4) inp
              $ prog // [(load 3, if operand 1 == operand 2 then 1 else 0)]

      -- halt
      99 -> []

  where load p = prog ! (pos + p)
        instr = load 0
        opcode = instr `mod` 100
        mode p = instr `div` (10 ^ (p + 1)) `mod` 10
        operand p = case mode p of
                      0 -> prog ! load p
                      1 -> load p

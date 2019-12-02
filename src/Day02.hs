-- file src/Day02.hs
module Day02 ( parseInput
             , part1
             , part2
             , execute
             ) where

import Data.Vector.Unboxed (Vector(..), fromList, (//), (!))
import Data.List (find)
import Data.List.Split (splitOn)

type Program = Vector Int

parseInput :: String -> Program
parseInput = fromList . map read . splitOn ","

part1 :: Program -> Int
part1 prog = execute prog' ! 0
  where prog' = prog // [(1, 12), (2, 2)]

part2 :: Program -> Int
part2 prog = head [ 100 * n + v
                  | n <- [0..99]
                  , v <- [0..99]
                  , go n v ! 0 == 19690720 ]
  where go n v = execute $ prog // [(1, n), (2, v)]

execute :: Program -> Program
execute = executeAt 0

executeAt :: Int -> Program -> Program
executeAt pos prog = case load pos of
                       1  -> executeAt (pos + 4) $ prog // [(out, op1 + op2)]
                       2  -> executeAt (pos + 4) $ prog // [(out, op1 * op2)]
                       99 -> prog
  where load = (prog !)
        op1 = load . load $ pos + 1
        op2 = load . load $ pos + 2
        out = load $ pos + 3

-- file src/Day13.hs
{-# LANGUAGE RecordWildCards #-}

module Day13 ( parseInput
             , part1
             , part2
             ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List.Split (chunksOf)
import Data.List (foldl')
import Computer

import Debug.Trace (trace)

type Coord = (Int, Int)

data Game = Game { ball :: Coord
                 , paddle :: Coord
                 , score :: Int }

data Instruction = Instruction { position :: Coord
                               , tile :: Int }

parseInput :: String -> Computer
parseInput = readProgram

part1 :: Computer -> Int
part1 = M.size
      . M.filter (== 2)
      . foldl' place M.empty
      . chunksOf 3
      . execute []
  where place m [x, y, tile] = M.insert (x, y) tile m

part2 :: Computer -> Int
part2 = play . store 0 2

play :: Computer -> Int
play computer = score $ last games
  where (games, moves) = unzip $ play' instructions
        instructions = map (\[x, y, t] -> Instruction (x, y) t)
                     . chunksOf 3
                     $ execute moves computer

        play' is = (game, joystick game) : move game is''
          where game = Game (position ball) (position paddle) 0
                (ball:is')    = findTile 4 is
                (paddle:is'') = findTile 3 is'
                findTile t = dropWhile ((/= t) . tile)

        move game [] = [(game, 0)]
        move game (i:is)
          | tile i == 4 = (game', joystick game') : move game' is
          | otherwise   = move game' is
          where game' = update i game

joystick :: Game -> Int
joystick (Game (xb, _) (xp, _) _) = signum (xb - xp)

update :: Instruction -> Game -> Game
update (Instruction pos       4)     game = game { ball = pos }
update (Instruction pos       3)     game = game { paddle = pos }
update (Instruction ((-1), 0) score) game = game { score = score }
update _                             game = game

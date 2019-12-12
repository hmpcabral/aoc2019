-- file src/Day12.hs
module Day12 ( parseInput
             , part1
             , part2
             ) where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Linear (V3(..), (^+^), (^-^), dot, basis)
import Data.List ((!!), findIndices)

data Planet = Planet { position :: V3 Int
                     , velocity :: V3 Int }
            deriving (Show, Eq)

parseInput :: String -> [Planet]
parseInput = map (planetAt . readPos) . lines
  where planetAt p = Planet p $ V3 0 0 0
        readPos = fst . head . readP_to_S coordinate

part1 :: [Planet] -> Int
part1 = totalEnergy . (!! 1000) . iterate step

part2 :: [Planet] -> Int
part2 system = foldr1 lcm $ map (period system . project) basis
  where project b ps = [(p `dot` b, v `dot` b) | Planet p v <- ps]

coordinate :: ReadP (V3 Int)
coordinate = do
  x <- string "<x=" >> number
  y <- string ", y=" >> number
  z <- string ", z=" >> number
  return $ V3 x y z

number :: ReadP Int
number = do
  sign <- option '+' (char '-')
  val <- fmap read $ munch1 isDigit
  return $ case sign of
             '+' -> val
             '-' -> (-val)

step :: [Planet] -> [Planet]
step = move . attract
  where move = map $ \(Planet p v) -> Planet (p ^+^ v) v
        attract ps = [foldr gravity p ps | p <- ps]
        gravity q p = p { velocity = velocity p ^+^ delta }
          where delta = fmap signum $ position q ^-^ position p

totalEnergy :: [Planet] -> Int
totalEnergy = sum . map energy
  where energy (Planet pos vel) = sum (fmap abs pos) * sum (fmap abs vel)

period :: Eq a => [Planet] -> ([Planet] -> a) -> Int
period seed f = (!! 1) . findIndices (\ps -> f ps == f seed) $ iterate step seed

-- file src/Day14.hs
module Day14 ( parseInput
             , part1
             , part2
             ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isLetter)
import Numeric.Search.Integer (searchFrom)

type Material = String

data Recipe = Recipe { amount :: Int
                     , ingredients :: [(Material, Int)] }

type Cookbook = Map Material Recipe
type Inventory = Map Material Int

parseInput :: String -> Cookbook
parseInput = M.fromList . map readRecipe . lines
  where readRecipe = fst . head . readP_to_S parseRecipe

part1 :: Cookbook -> Int
part1 recipes = oreFor recipes "FUEL" 1

part2 :: Cookbook -> Integer
part2 recipes = searchFrom ((> available) . ore . fromIntegral) 1 - 1
  where ore n = oreFor recipes "FUEL" n
        available = 1000000000000

parseRecipe :: ReadP (Material, Recipe)
parseRecipe = do
    ingredients <- sepBy ingredient (string ", ")
    (material, amount) <- string " => " >> ingredient
    return (material, Recipe amount ingredients)

  where ingredient = do
            amount <- number
            material <- string " " >> munch1 isLetter
            return (material, amount)

        number = fmap read $ munch1 isDigit

oreFor :: Cookbook -> Material -> Int -> Int
oreFor recipes x n = (produce recipes (x, n) M.empty) M.! "ORE"

produce :: Cookbook -> (Material, Int) -> Inventory -> Inventory
produce _       ("ORE", want) pantry = M.insertWith (+) "ORE" want pantry
produce recipes (what,  want) pantry
  | want <= have = M.insert what (have - want) pantry
  | otherwise    = foldr (produce recipes) pantry' ings'

  where pantry' = M.insert what (base * multiplier - need) pantry
        ings' = map (\(x, n) -> (x, n * multiplier)) ings

        multiplier = need `div` base + signum (need `mod` base)
        need = want - have
        have = M.findWithDefault 0 what pantry

        Recipe base ings = recipes M.! what

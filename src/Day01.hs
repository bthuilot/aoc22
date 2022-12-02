module Day01 (day01) where

import Interface ( DayPart )
import Utils.Lists ( dropAndGroup )
import Data.List ( sortBy )
import GHC.IO.Handle (hGetContents)
import Control.Monad ( (>=>) )

day01 :: [DayPart]
day01 = [
  hGetContents >=> (return . show . part1),
  hGetContents >=> (return . show . part2)
        ]

-- | 'EvlesSnacks' is a list of list of 'Int', where each int represents the calories of a snack
-- and each sub list represents the snacks a particular Elf holds
type ElvesSnacks = [[Int]]

part1 :: String -> Int
part1 = head . sortCalories . buildInput

part2 :: String -> Int
part2 = (sum . take 3) . sortCalories . buildInput

buildInput :: String -> ElvesSnacks
buildInput = map (map read) . (dropAndGroup (/= "") . lines)

-- | 'findMaxCalories' will take in an 'ElvesSnacks' and return the total calorie count
-- for the Elf with the most calories for all snacks
sortCalories :: ElvesSnacks -> [Int]
sortCalories = sortBy (flip compare) . map sum

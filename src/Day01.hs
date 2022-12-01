module Day01 where

import Interface
import Utils.Lists (dropAndGroup)
import Data.List (sortBy)

day01 :: [DayPart]
day01 = [
  show . head . sortCalories . buildInput, -- part 1
  show . (sum . take 3) . sortCalories . buildInput -- part 2
        ]

-- | 'EvlesSnacks' is a list of list of 'Int', where each int represents the calories of a snack
-- and each sub list represents the snacks a particular Elf holds
type ElvesSnacks = [[Int]]


buildInput :: String -> ElvesSnacks
buildInput = map (map read) . (dropAndGroup (/= "") . lines)

-- | 'findMaxCalories' will take in an 'ElvesSnacks' and return the total calorie count
-- for the Elf with the most calories for all snacks
sortCalories :: ElvesSnacks -> [Int]
sortCalories = sortBy (flip compare) . map sum

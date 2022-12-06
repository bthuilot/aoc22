{-|
Module      : Day03
Description : Advent of Code 2022, Day 3 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

For this day, both parts are parse into a list of 'RucksackGrouping' (a list of string).
The shared character between each 'RucksackingGrouping' is found and converted to a priority, and the final sum of all priorities is returned

The difference between part 1 and 2 is how elements are grouped, part 1 being split each line in half, and part 2 being group by every 3rd.

https://adventofcode.com/2022/day/3
-}
module Day03 (day03) where

import Interface ( DayRunner )
import Data.List (intersect)
import Data.Char (ord)
import Utils.Lists ( chunks )
import GHC.IO.Handle (hGetContents)

day03 :: DayRunner
day03 h = do
  contents <- hGetContents h
  let inputs = map ($ contents) [part1Grouping, part2Grouping]
  return $ map (show . prioritySum) inputs

-- | 'Rucksack' is a string of upper and lower case letters
type Rucksack = String

-- | 'RucksackGrouping' is a list of 'Rucksack', where each element
-- shares one common character between them
type RucksackGrouping = [Rucksack]


-- | 'part1Grouping' will parse the input string into a list of
-- 'RucksackGrouping', where each lines contains two equal sized
-- 'Rucksack's
part1Grouping :: String -> [RucksackGrouping]
part1Grouping = map splitLine . lines
  where
    splitLine l = let (xs, ys) = splitAt (length l `div` 2) l in [xs,ys]

-- | 'part2Grouping' will parse the input string into a into lines
-- then group every 3 lines together
part2Grouping :: String -> [RucksackGrouping]
part2Grouping = chunks 3 . lines


-- | 'prioritySum' computes the sum of priorities for a list of
-- 'RuckscakGrouping'
prioritySum :: [RucksackGrouping] -> Int
prioritySum = sum . map (charToPriority . findCommonChar)

-- | 'charToScore' will convert a 'Char' to its prority
charToPriority :: Char -> Int
charToPriority c
  | o > 64 && o < 91 = o - 38
  | otherwise = o - 96
  where
    o = ord c

-- | 'findCommonChar' will find the common 'Char' amoungst the
-- given 'RucksackGrouping'. NOTE: If more than one 'Char' is shared,
-- only one will be returned
findCommonChar :: RucksackGrouping -> Char
findCommonChar = head . foldl intersect ['A'..'z']

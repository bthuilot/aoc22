module Day03 where

import Interface ( DayPart )
import Data.List (intersect)
import Data.Char (ord)
import Utils.Lists ( chunks )

day03 :: [DayPart]
day03 = [
  show . prioritySum . part1Grouping,
  show . prioritySum . part2Grouping
  ]

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

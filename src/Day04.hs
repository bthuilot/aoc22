{-|
Module      : Day05
Description : Advent of Code 2022, Day 3 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

For this day, the input is first parsed into a tuple of two ranges, where each range is a tuple of the lower and upper bounds.

From there, the lists are filtered and counted for fully contained ranges and overlapped ranges, for each part respectfully.

https://adventofcode.com/2022/day/4
-}
module Day04 ( day04 ) where

import Interface ( DayRunner )
import Utils.Lists ( splitOn )
import GHC.IO.Handle (hGetContents)

day04 :: DayRunner
day04 h = do
  contents <- hGetContents h
  let i = parseInput contents
  return $ map (flip ($) i) [
    show . count fullyContained,
    show . count overlap
    ]

-- | 'Range' is a list of sequenctial integres that reprsents a range of integers
type Range = (Int, Int)

-- | 'AssignmentPair' is a pair of assignments to a 'Range'
type AssignmentPair = (Range, Range)

-- | 'parseInput' will parse the input string, where each line is an assignment pair seperated by ',' and each range is two integers seperated by '-'
parseInput :: String -> [AssignmentPair]
parseInput = map buildAssignment . lines
  where
    buildAssignment s = let (r1, r2) = splitOn ',' s in (buildRange r1, buildRange r2)
    buildRange s = let (i1, i2) = splitOn '-' s in (read i1, read i2)

-- | 'count' will return the amount of elements in a list that
-- pass a predicate
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- | 'fullyContained' will return true if one of the 'Range's inside the 'AssignmentPair' is fully contained in another
fullyContained :: AssignmentPair -> Bool
fullyContained (r1, r2) = covers r1 r2 || covers r2 r1
  where
    covers (s, e) (l, u) = s <= l && e >= u

-- | 'overlap' will return true if one of the 'Range's inside the 'AssignmentPair' is partially contained in the other
overlap :: AssignmentPair -> Bool
overlap (r1, r2) = partial r1 r2 || partial r2 r1
  where
    partial (s, _) (l, u) = (s >= l) && (s <= u)



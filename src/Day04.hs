module Day04 where

import Interface ( DayRunner )
import Utils.Lists ( splitOn )
import Data.List ( (\\), intersect )
import GHC.IO.Handle (hGetContents)

day04 :: DayRunner
day04 h = do
  contents <- hGetContents h
  let i = parseInput contents
  return $ map (flip ($) i) [
    show . countFullyContained,
    show . countOverlaps
    ]

-- | 'Range' is a list of sequenctial integres that reprsents a range of integers
type Range = [Int]

-- | 'AssignmentPair' is a pair of assignments to a 'Range'
type AssignmentPair = (Range, Range)

-- | 'parseInput' will parse the input string, where each line is an assignment pair seperated by ',' and each range is two integers seperated by '-'
parseInput :: String -> [AssignmentPair]
parseInput = map buildAssignment . lines
  where
    buildAssignment s = let (r1, r2) = splitOn ',' s in (buildRange r1, buildRange r2)
    buildRange s = let (i1, i2) = splitOn '-' s in [read i1..read i2]


-- | 'countFullContained' will count the amount of full contained
-- 'AssignmentPair's in a list
countFullyContained :: [AssignmentPair] -> Int
countFullyContained = length . filter fullContained
  where
    fullContained  (r1, r2) = null (r1 \\ r2) || null (r2 \\ r1)

-- | 'countOverlaps' will count the amount of overlaping 'AssignmentPair's in
-- a list
countOverlaps :: [AssignmentPair] -> Int
countOverlaps = length . filter overlaps
  where
    overlaps (r1, r2) = not (null $ r1 `intersect` r2) || not (null $ r2 `intersect` r1)



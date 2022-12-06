{-|
Module      : Day06
Description : Advent of Code 2022, Day 6 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

For this day, I parse the string using a sliding window to find a prefix of a given length with no duplicates.

Since the function was generic enought to take in a prefix length, the difference between part 1 and 2 is the marker size.

https://adventofcode.com/2022/day/6
-}
module Day06 ( day06 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.List (nub)


-- | 'MarkerSize' is the size of the signal marker
type MarkerSize = Int


day06 :: DayRunner
day06 h = do
  contents <- hGetContents h
  return $  map (show . ($ contents)) [markerIndex 4, markerIndex 14]

-- | 'markerIndex' will search a string for a marker of size 'MarkerSize' with no repeats
-- and return the index of the last character
markerIndex :: MarkerSize -> String -> Int
markerIndex size buf
  | length (nub marker) /= length marker = 1 + markerIndex size (drop 1 buf)
  | otherwise = size
  where
    marker = take size buf

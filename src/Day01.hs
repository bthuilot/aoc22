{-|
Module      : Day01
Description : Advent of Code 2022, Day 1 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

For this day, the input is parsed into a list of list of ints, and each sub list is summed, and the final list sorted.

part1 is the first element of the sorted list, while 
https://adventofcode.com/2022/day/1
-}
module Day01 (day01) where

import Interface ( DayRunner )
import Utils.Lists ( dropAndGroup )
import Data.List ( sortBy )
import GHC.IO.Handle (hGetContents)

day01 :: DayRunner
day01 h = do
  contents <- hGetContents h
  let input = buildInput contents
  let sorted = sortTotalCalories input
  return [
    show (head sorted),
    show (sum $ take 3 sorted)
    ]


-- | 'EvlesSnacks' is a list of list of 'Int', where each int represents the calories of a snack
-- and each sub list represents the snacks a particular Elf holds
type ElvesSnacks = [[Int]]

-- | 'buildInput' will convert a string into a 'ElvesSnacks' 
buildInput :: String -> ElvesSnacks
buildInput = map (map read) . (dropAndGroup (/= "") . lines)

-- | 'sortTotalCalories' will take in an 'ElvesSnacks' and return the total calorie count for each grouping, sorted
sortTotalCalories :: ElvesSnacks -> [Int]
sortTotalCalories = sortBy (flip compare) . map sum

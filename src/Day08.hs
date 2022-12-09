{-|
Module      : Day08
Description : Advent of Code 2022, Day 8 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/8
-}
module Day08 ( day08 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.Char (digitToInt)
import Data.List (transpose)


day08 :: DayRunner
day08 h = do
  contents <- hGetContents h
  let grid = map (map digitToInt) $ lines contents
  return $ map ($ grid) [show . countVisible, show . maxScenicScore]


-- | 'countVisible' will count the amount of trees that are visible, meaning there are no trees with a height
-- greater than it in at least one grid direction
countVisible :: [[Int]] -> Int
countVisible grid = sum $ zipWith (countRow tp []) grid [0..]
  where
    tp =  transpose grid

-- | 'maxScenicScore' will return the max scenic score of all trees in a grid
maxScenicScore :: [[Int]] -> Int
maxScenicScore grid = maxAll $ zipWith (maxScenicRow tp []) grid [0..]
  where
    tp =  transpose grid

-- | 'maxAll' will find the max of a list of Ints
maxAll :: [Int] -> Int
maxAll = foldl max (-1)

-- | 'countRow' will count the total visible trees in a row
countRow :: [[Int]] -> [Int] -> [Int] -> Int -> Int
countRow (col : cols ) w (tree : e) r = score [nMax, sMax, eMax, wMax] + countRow cols (tree : w) e r
  where
    (n, s) = splitAt r col
    (eMax,wMax) = (maxAll e, maxAll w)
    (nMax,sMax) = (maxAll n, maxAll $ drop 1 s)
    score l
      | any (tree >) l = 1
      | otherwise = 0
countRow _ _ _ _ = 0


-- | 'countScenicTrees' will count the amount of scenic trees in a list for a given tree
countScenicTrees :: Int -> [Int] -> Int
countScenicTrees _ [] = 0
countScenicTrees i (x : xs)
  | i <= x = 1
  | otherwise = 1 + countScenicTrees i xs


-- | 'maxScenicRow' will return the max scenic score for a row
maxScenicRow :: [[Int]] -> [Int] -> [Int] -> Int -> Int
maxScenicRow (col : cols) w (tree : e) r = max scenicScore (maxScenicRow cols (tree : w) e r)
  where
    (n, s) = splitAt r col
    countTrees = countScenicTrees tree
    (eScore,wScore) = (countTrees e, countTrees w)
    (nScore,sScore) = (countTrees (reverse n), countTrees $ drop 1 s)
    scenicScore = product [nScore, sScore, eScore, wScore]
maxScenicRow _ _ _ _ = 0

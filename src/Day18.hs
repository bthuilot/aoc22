{-|
Module      : Day18
Description : Advent of Code 2022, Day 18 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/18
-}

module Day18 ( day18 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Utils.Lists (splitOnAll)
import qualified Data.Set as S


day18 :: DayRunner
day18 h = do
  contents <- hGetContents h
  let input = parseInput contents
  return [show $ parseInput contents]

type Point3D = (Int, Int, Int)

parseInput :: String -> [Point3D]
parseInput = map (parseLine . map read . splitOnAll ",") . lines
  where
    parseLine [x,y,z] = (x,y,z)
    parseLine _ = (0,0,0)


calcSurfaces :: S.Set Point3D -> Int -> Int
calcSurfaces
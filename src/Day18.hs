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
  let points = S.fromList $ parseInput contents
  return [show $ calcSurfaces points 0]

type Point3D = (Int, Int, Int)

parseInput :: String -> [Point3D]
parseInput = map (parseLine . map read . splitOnAll ",") . lines
  where
    parseLine [x,y,z] = (x,y,z)
    parseLine _ = (0,0,0)


calcSurfaces :: S.Set Point3D -> Int -> Int
calcSurfaces s total
  | S.null s = total
  | otherwise = calcSurfaces s' total'
  where
    cur = S.elemAt 0 s
    (seen, area) = calcClusterSurfaces s [cur]
    s' = S.difference s seen
    total' = total + area 

calcClusterSurfaces :: S.Set Point3D -> [Point3D] -> (S.Set Point3D, Int)
calcClusterSurfaces all queue = (S.empty, 0)


dirFuncs :: [Point3D -> Point3D]
dirFuncs = [\(x,y,z) -> (x + 1, y, z),
            \(x,y,z) -> (x - 1, y, z),
            \(x,y,z) -> (x, y + 1, z),
            \(x,y,z) -> (x, y - 1, z),
            \(x,y,z) -> (x, y, z + 1),
            \(x,y,z) -> (x, y, z - 1)]
             

findNeighbors :: S.Set Point3D -> Point3D -> [Point3D]
findNeighbors s p = foldl findNeighbors' [] dirFuncs
  where
    findNeighbors' n f
      | S.member (f p) s = f p : n
      | otherwise = n

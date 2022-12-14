{-|
Module      : Day15
Description : Advent of Code 2022, Day 15 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/15
-}

module Day15 ( day15 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.Char (isDigit)
import Utils.Lists (splitBy)
import Data.Bifunctor ( bimap )
import Debug.Trace (trace)


day15 :: DayRunner
day15 h = do
  contents <- hGetContents h
  let (p1Row, (p2Bound, signal)) = bimap read (bimap read parseSignals . splitBy "\n\n") $ splitBy "\n\n" contents
  print (p2Bound :: Int)
  let rs = countEmpty p1Row signal
  return [
    show $ rangesLen rs,
    show $ tuningFreq $ head $  getPossiblePoints (0, p2Bound) signal
    ]

type Point = (Int, Int)

type Distance = Int

type Signal = (Point, Point, Distance)

type Range = (Int, Int)

type Ranges = [Range]

parseSignals :: String -> [Signal]
parseSignals = foldl parseSignal [] . lines

parseSignal :: [Signal] -> String -> [Signal]
parseSignal acc s = sig : acc
  where
    sig = buildSignal $ parseSignal' s
    parseSignal' =  map read . filter (not . null) . map (filter intChar) . words
    intChar c =  isDigit c || c == '-'

buildSignal :: [Int] -> Signal
buildSignal [sX, sY, bX, bY] = (sP, bP, calcNycDis sP bP)
  where
    sP = (sX, sY)
    bP = (bX, bY)
buildSignal _ = error "invalid input"

-- get it
calcNycDis :: Point -> Point -> Int
calcNycDis (sX, sY) (bX, bY) =  abs (bX - sX) + abs (bY - sY)

countEmpty :: Int -> [Signal] -> Ranges
countEmpty _ [] = []
countEmpty i (((x, y), (bX, bY), d) : ss)
  | d < h = recur
  | otherwise = foldr insertRange l recur
  where
    h = abs(i - y)
    amt = abs(d - h)
    r = (x - amt, x + amt)
    recur = countEmpty i ss
    l = if bY == i then splitRange r bX else [r]

rangesLen :: Ranges -> Int
rangesLen = foldl rangeLen 0
  where
    rangeLen acc (x,y) = acc + 1 + abs (x-y)


insertRange :: Range -> Ranges -> Ranges
insertRange p [] = [p]
insertRange  r@(xMin, xMax) rs@(r'@(lX, uX) : rs')
  | xMax < lX  = r : rs
  | xMin > uX = r' : insertRange r rs'
  | otherwise = insertRange (min xMin lX, max uX xMax) rs'

splitRange :: Range -> Int -> [Range]
splitRange r@(x,y) i
  | x == y && i == x = []
  | i == x = [(x + 1, y)]
  | i == y = [(x, y -1)]
  | i < x || i > y = [r]
  | otherwise = [(x, i - 1), (i + 1, y)]


subOverlap :: [Range] -> Range -> [Range]
subOverlap [] _ = []
subOverlap (b@(bX, bY) : bs) s@(sX, sY)
  | sX <= bX && sY >= bY = recur
  | sX > bX && sY < bY = (bX, sX - 1) : (sY + 1, bY) : recur
  | sX > bX && sY >= bY = (bX, sX - 1) : recur
  | sY < bY && sX <= bX = (sY + 1, bY) : recur
  | trace (show b) False = undefined
  | otherwise = b : recur
  where
    recur = subOverlap bs s


rangesToPoints :: Int -> [Range] -> [Point]
rangesToPoints _ [] = []
rangesToPoints y ((l, u) : rs) = foldl (\acc x -> (x,y) : acc) recur [l..u]
  where
    recur = rangesToPoints y rs


getPossiblePoints :: Range -> [Signal] -> [Point]
getPossiblePoints r@(l,u) s = filter (isNotBeacon s) allPoints
  where
    pointsAtY y = rangesToPoints y $ foldl subOverlap [r] (countEmpty y s)
    allPoints = foldl (\acc y -> pointsAtY y ++ acc) [] [l..u]
    isNotBeacon [] _ = True
    isNotBeacon ((_, b, _): bs) p = p /= b && isNotBeacon bs p

tuningFreq :: Point -> Int
tuningFreq (x, y) = x * 4000000 + y

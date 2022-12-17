{-|
Module      : Day12
Description : Advent of Code 2022, Day 12 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/12
-}
module Day12 ( day12 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.Char (ord)
import qualified Data.Map.Strict as M
import qualified Data.Set as S


day12 :: DayRunner
day12 h = do
  contents <- hGetContents h
  let g@(G _ s s' _ _) = parseGraph contents
  return [
    show $ shortestPath (S.singleton s) g [(s, 0)],
    show $ minimum $ filter (/= 0)  $ map (\p -> shortestPath (S.singleton p) g [(p, 0)]) s'
    ]
   

type Elevation = Int

type ElevationMap = M.Map Point Elevation

type Seen = S.Set Point

type Point = (Int, Int)

type Bounds = (Int, Int)

data Graph
  = G
  ElevationMap -- ^ 'ElevationMap' is a map from 'Point' to 'Elevation'
  Point -- ^ 'Point' is the starting point
  [Point] -- ^ '[Point]' is a list of other starting points
  Point -- ^ 'Point' is the ending point
  Bounds
  deriving (Show, Eq)


parseGraph :: String -> Graph
parseGraph s = parseGraph' start (0,0) l
  where
    l = lines s 
    start = G M.empty (0,0) [] (0,0) (length (head l) - 1, length l - 1)

parseGraph' :: Graph -> Point -> [[Char]] -> Graph
parseGraph' g _ [] = g
parseGraph' g (x, y) (r : rs) = parseGraph' g' (x, y+1) rs
  where
    g' = parseRow g (x, y) r


parseRow :: Graph -> Point -> [Char] -> Graph
parseRow g _ [] = g
parseRow (G m s s' e b@(bX, bY)) p@(x, y) (c : cs)
  | c == 'S' = parseRow (G (M.insert p (ord 'a') m) p s'' e b) (x + 1, y) cs
  | c == 'E' = parseRow (G (M.insert p (ord 'z') m) s s'' p b) (x + 1, y) cs
  | otherwise = parseRow (G (M.insert p (ord c) m) s s'' e b) (x + 1, y) cs
    where
      isStartingEdge = c == 'a' && (x == bX || x == 0 || y == 0 || y == bY)
      s'' = if isStartingEdge
            then p : s'
            else s'


shortestPath :: Seen -> Graph -> [(Point, Int)] -> Int
shortestPath _ _ [] = 0
shortestPath seen g@(G m _ _ e _) ((p@(x, y), s) : ps)
  | p == e = s
  | otherwise = shortestPath seen' g ps'
  where
    neighbors = filter (`S.notMember` seen) [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]
    seen' = foldr (S.insert . fst) seen newQueue
    curElevation = (M.!) m p
    newQueue = foldl (buildQueue (m, curElevation, s + 1)) [] neighbors
    ps' = ps ++ newQueue

buildQueue :: (ElevationMap, Elevation, Int) -> [(Point, Int)] -> Point -> [(Point, Int)]
buildQueue (m, e, s) queue p
  | not (M.member p m) = queue
  | dis < 2 = (p, s) : queue
  | otherwise = queue
  where
    dis = (M.!) m p - e

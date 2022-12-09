{-|
Module      : Day09
Description : Advent of Code 2022, Day 9 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/9
-}
module Day09 ( day09 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import qualified Data.Set as Set

day09 :: DayRunner
day09 h = do
  contents <- hGetContents h
  let inst = parseInput contents
  let f = show . Set.size . performMoves Set.empty inst . flip replicate (1,1)
  return $ map f [2,10]

-- | 'Point' represents the location of a knot
type Point = (Int, Int)

-- | 'Rope' represents a rope where each 'Point' in the list is a location of a knot
type Rope = [Point]

-- | 'UpdateFunc' is a function to update the position of a 'Point'
type UpdateFunc = Point -> Point

-- | 'Movement' represents a movement of the a knot in a rope, where the position
-- of the knot is updated by 'UpdateFunc', 'Int' amount of times
data Movement = Move UpdateFunc Int


-- | 'parseInput' will parse the given string into a list of 'Movement'
parseInput :: String -> [Movement]
parseInput = map parseMove . lines

-- | 'parseMove' will parse a 'Movement' from the given string
parseMove :: String -> Movement
parseMove s =
  let (dir, amt) = splitAt 1 s
  in Move (parseDir dir) (read $ drop 1 amt)
  where
    parseDir "L" = \(x,y) -> (x - 1, y)
    parseDir "R" = \(x,y) -> (x + 1, y)
    parseDir "U" = \(x,y) -> (x, y + 1)
    parseDir _ = \(x,y) -> (x, y - 1)

-- | 'performMoves' will perform a list of moves onto the the given 'Rope', and update the
-- given set will a locations of the tail of the rope
performMoves :: Set.Set Point -> [Movement] -> Rope -> Set.Set Point
performMoves s (move : moves) rope = performMoves newS moves newRope
  where
    (newS, newRope) = performMove s move rope
    -- newT = updateTail newH t
performMoves s _ _ = s

-- | 'performMove' will perform a 'Movement' on a rope and return the updated rope.
performMove :: Set.Set Point -> Movement -> Rope  -> (Set.Set Point, Rope)
performMove set (Move f amt) rope = foldl performMove' (set, rope) [1..amt]
  where
    performMove' (s, h : r) _ = updateRope s (f h : r)
    performMove' (s, r) _ = (s,r)


-- | 'updateRope' will traverse through a 'Rope', updating each succedding element, based on the
-- position of the prior element. Will record the new location of the tail of the repo in the given set
updateRope :: Set.Set Point -> Rope -> (Set.Set Point, Rope)
updateRope s [h, t] = (Set.insert newT s, [h, newT])
  where
    newT = updateKnot h t
updateRope s (k1 : k2 : ks) = (newS, k1 : newKnots)
  where
    newK2 = updateKnot k1 k2
    (newS, newKnots) = updateRope s (newK2 : ks)
updateRope s ks = (s, ks)


updateKnot :: Point -> Point -> Point
updateKnot (hX, hY) t@(tX, tY)
  | xDis <= 1 && yDis <= 1 = t
  | xDis == 2 && yDis == 0 = ((hX + tX) `div` 2, tY)
  | xDis == 0 && yDis == 2 = (tX, (hY + tY) `div` 2)
  | otherwise = (tX + if hX > tX then 1 else (-1), tY + if hY > tY then 1 else (-1))
  where
    xDis = abs (hX - tX)
    yDis = abs (hY - tY)

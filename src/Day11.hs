{-|
Module      : Day11
Description : Advent of Code 2022, Day 11 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

Solution is a little hacky in places but works!

https://adventofcode.com/2022/day/11
-}
module Day11 ( day11 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.List (sortBy)
import Utils.Lists (splitOnAll)
import Data.Maybe (fromJust, isNothing)
import Debug.Trace ()
import qualified Data.Map.Strict as Map


day11 :: DayRunner
day11 h = do
  contents <- hGetContents h
  let [msP1, msP2] = map (parseInput contents) [(`div` 3), id]
  return $ map (show . product . take 2 . countInspected) [
    performRounds 20 msP1,
    performRounds 10000 msP2
    ]

type Op = Int -> Int

type Item = Int

data Monkey = M {
  items :: [Item],
  op :: Op,
  test :: (Int, Int, Int),
  inspected :: Int
}

type Monkeys = Map.Map Int Monkey

performTest :: Monkey -> Item -> Int
performTest M{test=(d,t,f)} i
  | i `rem` d == 0 = t
  | otherwise = f

parseInput :: String -> Op -> Monkeys
parseInput s o = parseInput' 0 Map.empty $ splitOnAll "\n\n" s
  where
    parseInput' _ m [] = m
    parseInput' i m (l : ls) = parseInput' (i + 1) (Map.insert i (parseMonkey o $ lines l) m) ls


parseMonkey :: Op -> [String] -> Monkey
parseMonkey o [_, startStr, opStr, test, testT, testF]
  = M{
  items=parseStartItems startStr,
  op=o . parseOperation opStr,
  test=parseTest (test, testT, testF),
  inspected=0
  }
parseMonkey _ _ = error "invalid input"

parseStartItems :: String -> [Int]
parseStartItems = map read . splitOnAll ", " . drop 18

parseOperation :: String -> (Int -> Int)
parseOperation s = \old -> parseOp o old (parseVal old valS)
  where
    (o, valS) = splitAt 1 $ drop 23 s
    parseVal old " old" = old
    parseVal _ n = read n
    parseOp "+" = (+)
    parseOp _ = (*)

parseTest :: (String, String, String) -> (Int, Int, Int)
parseTest (pS, tS, fS) = (p,t,f)
  where
    p = read $ drop 21 pS
    t = read $ drop 29 tS
    f = read $ drop 30 fS

countInspected :: Monkeys -> [Int]
countInspected = sortBy (flip compare) . Map.foldl (\l m -> inspected m : l) []

performRounds :: Int -> Monkeys -> Monkeys
performRounds rounds ms = performRounds' rounds ms
  where
    maxI = Map.foldl (\p M{test=(d,_,_)} ->p * d) 1 ms
    performRounds' 0 ms' = ms' 
    performRounds' i ms' = performRounds (i - 1) $ performTurns maxI 0 ms'

performTurns :: Int -> Int -> Monkeys -> Monkeys
performTurns maxI i ms
  | not $ Map.member i ms = ms
  | otherwise = performTurns maxBound (i + 1) ms'
  where
    (m', passedItems) = inspectItems maxI ((Map.!) ms i)
    ms' = passItems (Map.insert i m' ms) passedItems


inspectItems :: Int -> Monkey -> (Monkey, [(Int, Item)])
inspectItems maxI m = (m{items=[], inspected=inspected m + length thrownItems}, thrownItems)
  where
    thrownItems = map (\i -> let i' = op m i `rem` maxI in (performTest m i', i')) (items m)

passItems :: Monkeys -> [(Int, Item)] -> Monkeys
passItems ms [] = ms
passItems ms ((mNum, i) : is)
  | isNothing maybeM = passItems ms is
  | otherwise = passItems (Map.insert mNum m{items=items m ++ [i]} ms) is
 where
   maybeM = Map.lookup mNum ms
   m = fromJust maybeM
   

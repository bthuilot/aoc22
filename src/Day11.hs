{-|
Module      : Day11
Description : Advent of Code 2022, Day 11 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/11
-}
module Day11 ( day11 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.List (sort, sortBy)
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


type Op = Integer -> Integer

data Monkey = M {
  items :: [Integer],
  op :: Op,
  test :: (Integer, Int, Int),
  inspected :: Int
}

showMM :: Monkey -> String
showMM M{items=is, test=ts, op=p, inspected=i} = show is ++ "  " ++ show ts ++ " " ++ " | "++show i 

showM :: Monkeys -> String
showM = Map.foldlWithKey (\a i m -> show i ++ " " ++ showMM m ++ "\n" ++ a) ""

type Monkeys = Map.Map Int Monkey

performTest :: Monkey -> Integer -> Int
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

parseStartItems :: String -> [Integer]
parseStartItems s
  -- | trace (show s) False = undefined
  | otherwise = (map read . splitOnAll ", " . drop 18) s

parseOperation :: String -> (Integer -> Integer)
parseOperation s
  -- | trace (show s) False = undefined
  | otherwise = \old -> parseOp o old (parseVal old valS)
  where
    (o, valS) = splitAt 1 $ drop 23 s
    parseVal old " old" = old
    parseVal _ n = read n
    parseOp "+" = (+)
    parseOp _ = (*)

parseTest :: (String, String, String) -> (Integer, Int, Int)
parseTest (pS, tS, fS) = (p,t,f)
  where
    p = read $ drop 21 pS
    t = read $ drop 29 tS
    f = read $ drop 30 fS

countInspected :: Monkeys -> [Int]
countInspected = sortBy (flip compare) . Map.foldl (\l m -> inspected m : l) []

performRounds :: Int -> Monkeys -> Monkeys
performRounds 0 ms = ms
performRounds i ms
  -- | trace (showM ms) False = undefined
  | otherwise = performRounds (i - 1) ms'
  where
    ms' = performTurns 0 ms

performTurns :: Int -> Monkeys -> Monkeys
performTurns i ms
  | not $ Map.member i ms = ms
  | otherwise = performTurns (i + 1) ms'
  where
    (m', passedItems) = inspectItems ((Map.!) ms i)
    ms' = passItems (Map.insert i m' ms) passedItems


inspectItems :: Monkey -> (Monkey, [(Int, Integer)])
inspectItems m
  -- | trace (show thrownItems) False = undefined
  | otherwise = (m{items=[], inspected=inspected m + length thrownItems}, thrownItems)
  where
    thrownItems = map (\i -> let i' = op m i in (performTest m i', i')) (items m)

passItems :: Monkeys -> [(Int, Integer)] -> Monkeys
passItems ms [] = ms
passItems ms ((mNum, i) : is)
  | isNothing maybeM = passItems ms is
  | otherwise = passItems (Map.insert mNum m{items=items m ++ [i]} ms) is
 where
   maybeM = Map.lookup mNum ms
   m = fromJust maybeM
   

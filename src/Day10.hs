{-|
Module      : Day10
Description : Advent of Code 2022, Day 10 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/10
-}
module Day10 ( day10 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.List (isPrefixOf, intercalate)
import Utils.Lists (chunks)

day10 :: DayRunner
day10 h = do
  contents <- hGetContents h
  return $ map ($ (buildExec (0,1) . parseInput) contents) [
    show . sum . flip calcSignals [20, 60, 100, 140, 180, 220],
    showCRT . drawCRT
    ]
    
-- | 'Inst' represents an instruction for the CPU
data Inst
  = NOOP -- ^ 'NOOP' is a operation that takes 1 cycle
  | ADDX Int -- ^ 'ADDX' adds a given value to register X and takes 2 cycles

-- | 'Program' is a list of a 'Inst' to be performed
type Program = [Inst]

-- | 'Register' is the value of the register
type Register = Int

-- | 'Cycle' is a the cycle value of the CPU
type Cycle = Int

-- | 'Tick' is a CPU tick, represented as a pair of the current cycle and register value
type Tick = (Cycle, Register)

-- | 'Exec' is the list of ticks for a program execution
type Exec = [Tick]

-- | 'CRT' is a CRT display
type CRT = String

-- | 'showCRT' renders a CRT display with newlines
showCRT :: CRT -> String
showCRT = ("\n" ++) . intercalate "\n" . chunks 40


parseInput :: String -> Program
parseInput = map parseInst . lines
  where
    parseInst s
      | "addx" `isPrefixOf` s = ADDX $ (read . drop 5) s
      | otherwise = NOOP

-- | 'buildExec' will perform the instructions in 'Program' on a 'Tick' and
-- return the generated 'Exec'
buildExec :: Tick -> Program -> Exec
buildExec _ [] = []
buildExec (c, r) (NOOP : insts) = (c + 1, r) : buildExec (c + 1, r) insts
buildExec (c, r) (ADDX x : insts) = (c + 1, r) : (c + 2, r) : buildExec (c + 2, r + x) insts

-- | 'drawCRT' will draw a 'CRT' from a program 'Exec'
drawCRT :: Exec -> CRT
drawCRT [] = ""
drawCRT ((c, r) : rs)
  | abs diff < 2 =  '#' : recur
  | otherwise = '.' : recur
  where
    diff = ((c - 1) `rem` 40) - r
    recur = drawCRT rs


-- | 'calcSignals' will calculate the singals at each of the supplied
-- 'Cycle' breakpoints 
calcSignals :: Exec -> [Cycle] -> [Int]
calcSignals [] _ = []
calcSignals _ [] = []
calcSignals ((c,r) : regs) breakpoints@(b : bs)
  | c == b = c * r : calcSignals regs bs
  | otherwise = calcSignals regs breakpoints


{-|
Module      : Day05
Description : Advent of Code 2022, Day 5 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

For this day, the input is parsed into a list of 'Stack' and a list of 'Instruction'.

For each of the parts, the list of instructions is performed on the list of stacks, with part 1 being reversed before appened to the front of the stack when moved.

https://adventofcode.com/2022/day/5
-}
module Day05 ( day05 ) where

import Interface ( DayRunner )
import Utils.Lists ( splitBy, chunks, dropEvery)
import Data.List (transpose)
import GHC.IO.Handle (hGetContents)

day05 :: DayRunner
day05 h = do
  contents <- hGetContents h
  return $ map (\f ->
                  getTopOfStacks $  f (parseInput contents)
               ) [runModel9000, runModel9001]

-- | 'Instruction' is a data type for stack instructions
data Instruction
  -- | 'Move' represents a movement of items from one stack to another
  = Move 
  Int -- 
  Int
  Int

-- | 'Stack' is a list of characters
type Stack = [Char]

-- | 'parseInput' will parse the input into a list of 'Stack's and a listof 'Instruction'.
-- Parses by spliting the input on 2 consecutive new lines, then parsing
-- the first half into a list of stacks and the second into instructions
parseInput :: String -> ([Stack], [Instruction])
parseInput s = (parseStacks stacksS, parseInstrs instrsS)
  where
    (stacksS, instrsS) = splitBy "\n\n" s

-- | 'parseStacks' will parse an input string into a list of 'Stack's by first remove the last line (the stack labels),
-- Then for each line, the 4th char is dorpped and grouped by 3, then transposed. 
-- Finally, the elements are filtered and parsed into chars
parseStacks :: String -> [Stack]
parseStacks s = map (filter (/= ' ') . map parseStackElem) stacks
  where
    dropLabels = init $ lines s
    stacks = transpose $ map (chunks 3 . dropEvery 4) dropLabels

-- | 'parseInstrs' will parse a input string into a list of instructions.
-- Will error if the input string is not the correct format
parseInstrs :: String -> [Instruction]
parseInstrs = map parseLine . lines
  where
    -- This section just assumes valid input
    -- lets pray its always valid 
    parseLine l = let [_, amt, _, from, _, to] = words l 
                  in Move (read amt) (read from) (read to)

-- | 'parseStackElem' will parse a string from '[c]' into 'c' for all c.
-- Will parse into ' ' if string does not match
parseStackElem :: String -> Char
parseStackElem ['[', c, ']'] = c
parseStackElem _ = ' '


-- | 'CrateModelF' is a function that is applied to a 'Stack', when it is appened to the front of another. 
type CrateModelF = Stack -> Stack


-- | 'runInstr' will perform a 'Instruction' on a list of 'Stack' and return the new list of stacks.
-- Will error if instructions are not vali
runInstr :: CrateModelF -> [Stack] -> Instruction -> [Stack]
runInstr cm stacks (Move amt from to) = preT ++ (vals ++  tS) : postT
  where
    -- Again assuming valid input
    (preF, (f : postF)) = splitAt (from - 1) stacks
    (vals, fS) = (cm $ take amt f, drop amt f)
    (preT, tS : postT) = splitAt (to - 1) $  preF ++ (fS : postF)
    


-- | 'runModel9000' will run a list of 'Instruction' on a list of 'Stack'
-- and produce a new 'Stack' using the Model 9000's method of moving elements
runModel9000 :: ([Stack], [Instruction]) -> [Stack]
runModel9000 (s,i) = foldl (runInstr reverse) s i

-- | 'runMode9001' will run a list of 'Instruction' on a list of 'Stack'
-- and produce a new 'Stack' using the Model 9001's method of moving elements
runModel9001 :: ([Stack], [Instruction]) -> [Stack]
runModel9001 (s, i) = foldl (runInstr id) s i

-- | 'getTopOfStacks' will return the 'Char' at the head of every non empty stack
getTopOfStacks :: [Stack] -> [Char]
getTopOfStacks [] = []
getTopOfStacks ([] : xs) = getTopOfStacks xs
getTopOfStacks (ys : xs) = head ys : getTopOfStacks xs

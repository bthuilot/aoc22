{-|
Module      : Day02
Description : Advent of Code 2022, Day 2 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

For this day I first parse the given input into a 'Strategy', which is
a list of pairings of the opponents shape and my shape for each round.

To share common functionailty between both parts, the 'parseStrategy'
function takes in a 'StrategyDecrypter' which is used to parse what shape
the X, Y, and Z corresponds to.

One a 'Strategy' is parsed, both parts use the same calculating function
to get the score.

https://adventofcode.com/2022/day/2
-}
module Day02 (day02) where

import Interface (DayPart)

day02 :: [DayPart]
day02 = [
--  show . calculateStrategyScore . parseStrategy p1Decrypter,
 -- show . calculateStrategyScore . parseStrategy p2Decrypter
  ]

-- | 'Shape' represents the possible shapes to throw when playing
-- rock paper scissors
data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

-- | 'Round' represents a round of rock paper scissors, where the first
-- 'Shape' is the opponents shape and the second is " my " shape
type Round = (Shape, Shape)

-- | 'Strategy' represents the full game of rock paper scissors based on
-- strategy provided by the Elves
type Strategy = [Round]

-- | 'StrategyDecrypter' is a function to parse out what the X,Y or Z
-- corresponds to in a 'Strategy'. This is done because part 1 and 2
-- differ on what the strategy represents
type StrategyDecrypter = (String -> Shape -> Shape)

-- | 'shapeScore' returns the score for a shape
shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

-- | 'parseShape' wil parse what 'Shape' the A, B, or C represents in
-- the input
parseShape :: String -> Shape
parseShape "A" = Rock
parseShape "B" = Paper
parseShape _ = Scissors

-- | 'getWinningShape' will return the 'Shape' to use against the given
-- 'Shape' in order to win
getWinningShape :: Shape -> Shape
getWinningShape Rock = Paper
getWinningShape Paper = Scissors
getWinningShape Scissors = Rock

-- | 'getWinningShape' will return the 'Shape' that will lose against
-- the given 'Shape' 
getLosingShape :: Shape -> Shape
getLosingShape Rock = Scissors
getLosingShape Paper = Rock
getLosingShape Scissors = Paper

-- | 'outcomeScore' will return the score of a 'Round's outcome
outcomeScore :: Round -> Int
outcomeScore (other, mine)
  | other == mine = 3
  | getWinningShape other == mine = 6
  | otherwise = 0


-- | 'parseStrategy' will parse a given 'Strategy' from the input and
-- uses the given 'StrategyDecyrpter' to determine what the X, Y and Z
-- represents
parseStrategy :: StrategyDecrypter -> String -> Strategy
parseStrategy f = map parseRound . lines
  where
    parseRound s = let
      (xs, ys) = splitAt 1 s
      myShape = parseShape xs
      in (myShape, f  (tail ys) myShape)

-- | 'calculateStrategyScore' will calculare the score of the 'Strategy'
calculateStrategyScore :: Strategy -> Int
calculateStrategyScore = sum . map roundScore
  where
    roundScore r@(_, mine) = shapeScore mine + outcomeScore r

-- | 'p1Decrypter' decrypts the strategy for part 1, where X is Rock
-- Y is paper and Z is scissors
p1Decrypter :: StrategyDecrypter
p1Decrypter "X" = const Rock
p1Decrypter "Y" = const Paper
p1Decrypter _ = const Scissors

-- | 'p2Decrypter' decrypts the strategy for part 2, where X is lose, 
-- Y is draw and Z is win
p2Decrypter :: StrategyDecrypter
p2Decrypter "X" = getLosingShape 
p2Decrypter "Z" = getWinningShape
p2Decrypter _ = id

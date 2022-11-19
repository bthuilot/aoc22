module Challenges
    ( runDays
    ) where

import Interface
import Day0
import Control.Monad (foldM )
import Data.String (String)


getDay :: Int -> Day
getDay 0 = day0
getDay _ = NotImplementedDay

runDays :: [Int] -> [Result]
runDays = map (\i -> (runDay i . getDay) i )

getInput :: Int -> String
getInput _ = ""

runDay :: Int -> Day -> Result
runDay i NotImplementedDay= NotImplemented i
runDay i (Parts parts) = DayResult {
  num=i,
  results=map (\f -> f input) parts
  }
  where
    input = getInput i



module Challenges
    ( runDay, buildDay)
where

import Interface ( Result(..), Day(..), DayRunner )
import Day00 ( day00 )
import Day01 ( day01 )
import Day02 ( day02 )
import Day03 ( day03 )
import Day04 ( day04 )
import Day05 ( day05 )
import Day06 ( day06 )
import Day07 ( day07 )
import Day08 ( day08 )

import GHC.IO.IOMode (IOMode(ReadMode))
import Data.Functor ( (<&>) )
import System.IO ( openFile )
import System.Directory ( doesFileExist ) 



-- | 'getDayParts' will reutrn the parts for a given day number
getRunner :: Int -> DayRunner
getRunner 0 = day00
getRunner 1 = day01
getRunner 2 = day02
getRunner 3 = day03
getRunner 4 = day04
getRunner 5 = day05
getRunner 6 = day06
getRunner 7 = day07
getRunner 8 = day08
getRunner _ = const (return [])

-- | 'buildDay' will build a 'Day' from a function to retrieve its input and its date number
buildDay :: Int -> Day
buildDay n = Day n (getRunner n)

-- | 'getInput' will return the input for a day based on date number
getInput :: Int -> String
getInput = ("inputs/" ++) . show
  

-- | 'runDay' will run a 'Day' and return its 'Result'
runDay :: Day -> IO Result
runDay (Day n runner)  = do
  let fName = getInput n
  exists <- doesFileExist fName
  if exists
    -- TODO(return not run for empty days)
    then openFile fName ReadMode >>= runner <&> DayResult n
    else return $ NotRun n

 

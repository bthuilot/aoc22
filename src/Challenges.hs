module Challenges
    ( runDay, buildDay, getInput
    ) where

import Interface ( Result(..), Day(..), DayPart )
import Day00 ( day00 )
import Day01 ( day01 )
import Day02 ( day02 )
import Day03 ( day03 )
import Day04 ( day04 )


-- | 'getDayParts' will reutrn the parts for a given day number
getDayParts :: Int -> Maybe [DayPart]
getDayParts 0 = return day00
getDayParts 1 = return day01
getDayParts 2 = return day02
getDayParts 3 = return day03
getDayParts 4 = return day04
getDayParts _ = Nothing

-- | 'buildDay' will build a 'Day' from a function to retrieve its input and
-- its date number
buildDay :: (Int -> IO String) -> Int -> IO Day
buildDay f n =
  case getDayParts n of
    Nothing -> return $ NotImplementedDay n
    Just parts -> do
      dayInput <- f n    
      return $ Day n dayInput parts

-- | 'getInput' will return the input for a day based on date number
getInput :: Int -> IO String
getInput i = readFile ("inputs/" ++ show i)
  

-- | 'runDay' will run a 'Day' and return its 'Result'
runDay :: Day -> Result
runDay (NotImplementedDay i) = NotRun i
runDay (Day n input parts)  = DayResult n $ map ($ input) parts


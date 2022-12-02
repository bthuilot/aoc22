module Challenges
    ( runDay, buildDay
    ) where

import Interface ( Result(..), Day(..), DayPart )
import Day00 ( day00 )
import Day01 ( day01 )
import Day02 ( day02 )
import GHC.IO.Handle (Handle)


-- | 'getDayParts' will reutrn the parts for a given day number
getDayParts :: Int -> Maybe [DayPart]
getDayParts 0 = return day00
getDayParts 1 = return day01
getDayParts 2 = return day02
getDayParts _ = Nothing

-- | 'buildDay' will build a 'Day' from a function to retrieve its input and its date number
buildDay :: (Int -> IO Handle) -> Int -> IO Day
buildDay f n =
  case getDayParts n of
    Nothing -> return $ NotImplementedDay n
    Just parts -> do
      dayInput <- f n    
      return $ Day n dayInput parts
  

-- | 'runDay' will run a 'Day' and return its 'Result'
runDay :: Day -> IO Result
runDay (NotImplementedDay i) = return $ NotRun i
runDay (Day n h parts)  = DayResult n <$> mapM (flip ($) h) parts
 

module Challenges
    ( runDay, buildDay
    ) where

import Interface ( Result(..), Day(..), DayRunner )
import Day00 ( day00 )
import Day01 ( day01 )
import Day02 ( day02 )
import GHC.IO.Handle (Handle)


-- | 'getDayParts' will reutrn the parts for a given day number
getRunner :: Int -> Maybe DayRunner
getRunner 0 = return day00
getRunner 1 = return day01
getRunner 2 = return day02
getRunner _ = Nothing

-- | 'buildDay' will build a 'Day' from a function to retrieve its input and its date number
buildDay :: (Int -> IO Handle) -> Int -> IO Day
buildDay f n = do
  h <- f n
  return $ Day n h (getDayParts n)
  -- case getDayParts n of
  --   Nothing -> return $ NotImplementedDay n
  --   Just parts -> do
  --     dayInput <- f n    
  --     return $ Day n dayInput parts
  

-- | 'runDay' will run a 'Day' and return its 'Result'
runDay :: Day -> IO Result
runDay (NotImplementedDay i) = return $ DayResult i []
runDay (Day n h runner)  = runner h >>= (return  . DayResult n)

 

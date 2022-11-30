module Challenges
    ( runDay, buildDay, getInput
    ) where

import Interface ( Result(..), Day(..), DayPart )
import Day0


-- | 'getDayParts' will reutrn the parts for a given day number
getDayParts :: Int -> Maybe [DayPart]
getDayParts 0 = return day0 
getDayParts _ = Nothing

-- | 'buildDay' will build a 'Day' from a function to retrieve its input and its date number
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
runDay d = DayResult n $ map ( $ (input d)) (parts d)
  where
   n = num d


module Challenges
    ( runDay, buildDay, getInput
    ) where

import Interface ( Result(..), Day(..), DayPart )
import Day0


getDayParts :: Int -> Maybe [DayPart]
getDayParts 0 = return day0 
getDayParts _ = Nothing

buildDay :: (Int -> IO String) -> Int -> IO Day
buildDay f n =
  case getDayParts n of
    Nothing -> return $ NotImplementedDay n
    Just parts -> do
      dayInput <- f n    
      return $ Day n dayInput parts

getInput :: Int -> IO String
getInput i = readFile ("inputs/" ++ show i)
  

runDay :: Day -> Result
runDay (NotImplementedDay i) = NotImplemented i
runDay d = DayResult n $ map ( $ (input d)) (parts d)
  where
   n = num d


module Interface
  (Day(..), Result(..), DayRunner, PartResult)
where

import Data.List (intercalate)
import GHC.IO.Handle (Handle)

type PartResult = String

type DayRunner = Handle -> IO [PartResult]

-- | 'Day' represents a day challenge
data Day =
  -- | 'Day' is an implemented day
  Day Int DayRunner
  -- | 'NotImplementedDay' represents a day that is not defined yet
  -- | NotImplementedDay Int

-- | 'Result' is the result from running a 'Day'
data Result =
  DayResult Int [PartResult] -- ^ 'DayResult' is the results of an implemented day's parts
  | NotRun Int -- ^ 'NotRun' represents a day that was not implemented, thus not run

-- | 'showResults' will show the results from a day's parts
showResults :: Int -> [String] -> String
showResults day outcomes = intercalate "\n" $ header : hr : tabbedLines
  where
    header = "day " ++ show day ++ ":"
    hr = replicate (length header)  '='
    tabbedLines = map ("  " ++) outcomes


instance Show Result where
  show (NotRun day) = "day " ++ show day ++ ": not implemented\n"
  show (DayResult dn res) = showResults dn parts ++ "\n"
    where
      parts = zipWith (\i r -> "part" ++ show i ++ ": " ++ r) [1..] res
 

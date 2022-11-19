module Interface where


import Data.List (intercalate)

type DayPart = (String -> String)

data Day = Parts [DayPart]
  | NotImplementedDay


data Result = DayResult {
  num :: Int,
  results :: [String]
  }
  | NotImplemented Int


showResults :: Int -> [String] -> String
showResults day lines = intercalate "\n" $ header : hr : tabbedLines
  where
    header = "day " ++ show day ++ ":"
    hr = replicate (length header)  '='
    tabbedLines = map ("  " ++) lines


instance Show Result where
  show (NotImplemented day) = "day " ++ show day ++ ": not implemented\n"
  show DayResult{
    num=dayNum,
    results=res
    }=showResults dayNum parts ++ "\n"
    where
      parts = map ("part1: " ++) res 
 

module Interface where


import Data.List (intercalate)

type DayPart = (String -> String)

data Day = Day {
  num :: Int,
  input :: String,
  parts :: [DayPart]
  }
  | NotImplementedDay Int


data Result = DayResult Int [String]
  | NotImplemented Int


showResults :: Int -> [String] -> String
showResults day lines = intercalate "\n" $ header : hr : tabbedLines
  where
    header = "day " ++ show day ++ ":"
    hr = replicate (length header)  '='
    tabbedLines = map ("  " ++) lines


instance Show Result where
  show (NotImplemented day) = "day " ++ show day ++ ": not implemented\n"
  show (DayResult dn res) = (showResults dn parts) ++ "\n"
    where
      parts = map (\(i, r) -> "part" ++ (show i) ++ ": " ++  r) (zip [1..] res)
 

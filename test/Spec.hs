import Interface
import System.Directory
import ParseArgs
import Challenges
import Data.List (intercalate)
import System.Exit
import Control.Monad ((>=>))


-- | 'TestOutcome' represents the outcome from running a test
data TestOutcome
  = Passed -- | 'Passed' represents a test that passed (actual equals expected)
  | Failed -- | 'Failed' represents a test that failed (acutal did not meet expected)
  deriving (Eq)

instance Show TestOutcome where
  show Passed = "pass"
  show Failed = "failed"

-- | 'DayPartOutcome' represents the 'TestOutcome' and the actual and expected output
type DayPartOutcome = (TestOutcome, String, String)
  
-- | 'TestResult' represents the result of running a day's test
data TestResult
  -- | 'RanTest' represents a test that was ran, and each parts outcome
  = RanTest Int [DayPartOutcome]
  -- | 'SkippedTest' represents a day that was skipped
  | SkippedTest Int

instance Show TestResult where
  show (SkippedTest i) = "day " ++ show i ++ ": skipped"
  show (RanTest i outcomes) = "day " ++ show i ++ ":\n" ++ showOutcomes
    where
      showOutcome ((o, a, e), p) = "  part " ++ show p ++ ": " ++ show o
        ++ if o == Failed then  "\n  ex: '" ++ e ++ "' ac: '" ++ a ++ "'" else ""
        -- Will show expected vs actual
      showOutcomes = intercalate "\n" $ map showOutcome (zip outcomes [1..]) 

-- | 'Results' represents the outcome of running a full 'TestSuite'
data Results =
  Results {  -- ^ 'Results' is the record for the counts of test outcomes from all 'TestResult'
  total :: Int,
  success :: Int,
  skipped :: Int,
  failed :: Int,
  testResults :: [TestResult]
  }


-- | 'TestCase' represents a test case in the suite for a day
data TestCase =
  -- | 'DayTest'  represents a test for an implemented day 
  DayTest Day [String]
  -- { day :: Day -- ^ 'day' represents the implemented day
          -- | 'expected' is the list of expected output for each part. 
          -- each index of 'expected' corresponds to the 'DayPart' of the same index in 'Day'
          -- , expected :: [String] 
          -- }

-- | 'TestSuite' reprsents the full test suite to run
type TestSuite = [TestCase]

-- | 'getTestInput' will read the test input file for a given day number
getTestInput :: Int -> IO String
getTestInput i = readFile ("test/testcases/inputs/" ++ show i)

-- | 'buildTestSuite' will build the full 'TestSuite' from a list of days to run. 
-- returns IO since input and expected output is read from files
buildTestSuite :: [Int] -> IO TestSuite
buildTestSuite = mapM (buildTestDay >=> buildTestCase)

buildTestDay :: Int -> IO Day
buildTestDay date =  do
      fileExists <- doesFileExist ("test/testcases/inputs/" ++ show date)
      if fileExists then buildDay getTestInput date else return $ NotImplementedDay date

buildTestCase :: Day -> IO TestCase
buildTestCase d@(NotImplementedDay _) = return $ DayTest d []
buildTestCase d@(Day date _ parts) = do
  expected <- mapM (readFile . getExpectedFile) [0..length parts -1]
  return $ DayTest d expected
  where
    getExpectedFile i =  "test/testcases/expected/" ++ show date ++  ".part" ++ show (i + 1)
      
-- | 'runTestCase' will run a 'TestCase' and update the given results with the outcome
runTestCase :: TestCase -> Results -> Results
runTestCase (DayTest (NotImplementedDay i) _) results = results{
  skipped=skipped results + 1,
  total=total results + 1 ,
  testResults=SkippedTest i : testResults results
  }
runTestCase (DayTest (Day i input parts) e) result
  | all (\(o, _, _) -> o == Passed) tests = updatedRes{success=success updatedRes + 1}
  | otherwise = updatedRes{failed=failed updatedRes + 1}
  where
    dayParts = parts
    tests = buildOutcome $ zip (map ($ input) dayParts) e
    tr = RanTest i tests
    updatedRes = result{total=total result + 1 , testResults=tr : testResults result}

-- | 'buildOutcome' will build a list of 'DayPartOutcome' for
-- each pairing of acutal and expected results
buildOutcome :: [(String, String)] -> [DayPartOutcome]
buildOutcome ((a, e) : xs) = (outcome, a, e) : buildOutcome xs
  where
    outcome = if a == e then Passed else Failed
buildOutcome _ = []

-- | 'runTestSuite' will run the full test suite and return the 'Results'.
runTestSuite :: TestSuite -> Results
runTestSuite = foldr runTestCase  baseResults
  where
    baseResults = Results 0 0 0 0 []

-- | 'printResults' will print the 'Results' given to the screen.
-- will print either passed or failed for each 'DayPartOutcome'
printResults :: Results -> IO ()
printResults result = putStrLn $ header ++ (foldl showDays "" $ testResults result )
  where
    showDays acc x  = acc ++ "\n\n" ++ show x
    header = intercalate "\n" [
      "total: " ++ show (total result),
      "passed: " ++ show (success result),
      "failed: " ++ show (failed result),
      "skipped: " ++ show (skipped result)] ++ "\n"

-- | 'main' is the entry point for the test suite             
main :: IO ()
main = do
  putStrLn "Running Test Suite"
  testCases <- parseArgs >>= buildTestSuite
  let results = runTestSuite testCases
  _ <- printResults results
  exitWith $ if failed results == 0 then ExitSuccess else ExitFailure 1


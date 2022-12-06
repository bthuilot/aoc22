import Interface
import System.Directory ( doesFileExist )
import ParseArgs ( parseArgs )
import Challenges ( buildDay )
import Data.List (intercalate)
import System.Exit ( ExitCode(ExitFailure, ExitSuccess), exitWith )
import Control.Monad (foldM)
import System.IO (Handle, IOMode(ReadMode), openFile)


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

-- | 'TestSuite' reprsents the full test suite to run
type TestSuite = [Day]

-- | 'buildTestSuite' will build the full 'TestSuite' from a list of days to run. 
-- returns IO since input and expected output is read from files
buildTestSuite :: [Int] -> TestSuite
buildTestSuite = map buildDay

      
-- | 'runTestCase' will run a 'TestCase' and update the given results with the outcome
runTestCase :: Results -> Day -> IO Results
runTestCase res d@(Day i _) = do
  exists <- doesFileExist fName
  if exists
    then (openFile fName ReadMode) >>= runAndUpdate res d
    else return $ res{total=total res + 1, skipped=skipped res + 1, testResults=testResults res ++ [SkippedTest i]}
  where
    fName = ("test/testcases/inputs/" ++ show i)


runAndUpdate :: Results -> Day -> Handle -> IO Results
runAndUpdate res (Day i runner) h = do
  results <- runner h
  ex <- mapM (readFile . getExpectedFile) [0..length results - 1]
  let outcomes = zipWith (\a e -> (if a == e then Passed else Failed, a, e)) results ex
  let tr = RanTest i outcomes
  let updatedRes = res{total=total res + 1, testResults= testResults res ++ [tr]}
  return $ if all (\(o, _, _) -> o == Passed) outcomes
           then updatedRes{success=success res + 1}
           else updatedRes{failed=failed res + 1}
  where
    getExpectedFile part =  "test/testcases/expected/" ++ show i  ++  ".part" ++ show (part + 1)


-- | 'runTestSuite' will run the full test suite and return the 'Results'.
runTestSuite :: TestSuite -> IO Results
runTestSuite = foldM runTestCase baseResults
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
  results <- parseArgs >>= runTestSuite . buildTestSuite
  _ <- printResults results
  exitWith $ if failed results == 0 then ExitSuccess else ExitFailure 1


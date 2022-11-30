import Interface
import System.Directory
import ParseArgs
import Challenges
import Data.List (intercalate)
import System.Exit


data TestOutcome = Passed | Failed
  deriving (Eq)

instance Show TestOutcome where
  show Passed = "pass"
  show Failed = "failed"

type DayPartOutcome = (TestOutcome, String, String)

partOutcome :: DayPartOutcome -> TestOutcome
partOutcome (o, _, _) = o
  

data TestResult = RanTest Int [DayPartOutcome]
  | SkippedTest Int

instance Show TestResult where
  show (SkippedTest i) = "day " ++ show i ++ ": skipped"
  show (RanTest i outcomes) = "day " ++ show i ++ ":\n" ++ showOutcomes
    where
      showOutcome ((o, e, a), i) = "  part " ++ show i ++ ": " ++ show o
        -- ++ "\n\tex: '" ++ e ++ "' ac: '" ++ a ++ "'"
        -- "^ Will show expected vs actual
      showOutcomes = intercalate "\n" $ map showOutcome (zip outcomes [1..]) 

data Results = Results {
  total :: Int,
  success :: Int,
  skipped :: Int,
  failed :: Int,
  testResults :: [TestResult]
  }


data TestCase = DayTest {
  day :: Day,
  expected :: [String]
  }
  | NonExistantDay Int
  
type TestSuite = [TestCase]


getTestInput :: Int -> IO String
getTestInput i = readFile ("test/testcases/inputs/" ++ show i)

buildTestCases :: [Int] -> IO [TestCase]
buildTestCases = mapM buildCase
  where
    getExpectedFile d i =  "test/testcases/expected/" ++ show d ++  ".part" ++ show (i + 1)
    buildCase date = do
        fileExists <- doesFileExist ("test/testcases/inputs/" ++ show date)
        if fileExists then do
          d <- buildDay getTestInput date
          expect <- mapM (readFile . getExpectedFile date) [0..(length $ parts d) -1]
          return DayTest {day=d, expected=expect}
          else return $ NonExistantDay date
    
runTestCase :: TestCase -> Results -> Results
runTestCase (NonExistantDay i) results = results{
  skipped=skipped results + 1,
  total=total results + 1 ,
  testResults=SkippedTest i : testResults results
  }
runTestCase dt result
  | all (\(o, _, _) -> o == Passed) tests = updatedRes{success=success updatedRes + 1}
  | otherwise = updatedRes{failed=failed updatedRes + 1}
  where
    d = (day dt)
    tests = runTest (map ($ input d) (parts d)) (expected dt)
    tr = RanTest (num d) tests
    updatedRes = result{total=total result + 1 , testResults=tr : testResults result}


runTest :: [String] -> [String] -> [DayPartOutcome]
runTest (a : as) (e : es) = (outcome, a, e) : runTest as es
  where
    outcome = if a == e then Passed else Failed
runTest _ _ = []


runTestSuite :: TestSuite -> Results
runTestSuite = foldr runTestCase  baseResults
  where
    baseResults = Results 0 0 0 0 []

main :: IO ()
main = do
  putStrLn "Running Test Suite"
  testCases <- parseArgs >>= buildTestCases
  let results = runTestSuite testCases
  _ <- printResults results
  exitWith $ if (failed results) == 0 then ExitSuccess else ExitFailure 1
  

-- showDay :: TestResult -> String

  
printResults :: Results -> IO ()
printResults result = putStrLn $ header ++ (foldl showDays "" $ testResults result )
  where
    showDays acc x  = acc ++ "\n\n" ++ show x
    header = intercalate "\n" [
      "total: " ++ show (total result),
        "passed: " ++ show (success result),
        "failed: " ++ show (failed result),
        "skipped: " ++ show (skipped result)] ++ "\n"

    --            (foldl printDay "" trs)
    -- trs = testResults result
    -- printDay other (TestResult i outcome _) = other ++ "\nday " ++ show i ++ ": " ++ (show outcome) ++ "\n"
   

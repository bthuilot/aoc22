
data Results = TestResults {
  total :: Int,
  success :: Int,
  skipped :: Int,
  failed :: Int }

data TestCase = TestCase {
  day :: Int,
  expectedP1 :: String,
  expectedP2 :: String
  }


tests :: [TestCase]
tests = [
  TestCase {
      day = 0,
      expectedP1 = "hello, world!",
      expectedP2 = "HELLO, WORLD!"
      }
  ]


main :: IO ()
main = putStrLn "Test suite not yet implemented"


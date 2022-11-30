module ParseArgs (parseArgs, Runs) where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Exit (exitWith, ExitCode(ExitFailure))

type Runs = [Int]

-- | 'parseArgs' will parse the program arguments into 'Runs'.
-- will exit with status 1 if arguments are invalid
parseArgs :: IO Runs
parseArgs = do
  args <- getArgs
  checkArgsError $ parseRuns args

-- | 'parseRuns' Will parse 'Runs' from the command line arguments.
-- will return Nothing if arguments are invalid
parseRuns :: [String] -> Maybe Runs
parseRuns [] = return [1..25]
parseRuns (flag : xs)
  | flag `elem` ["--days", "-d"] = ints
  | otherwise = Nothing
  where
    ints = mapM readMaybe xs

-- | 'checkArgsError' will unwrap a maybe if its a Just or exit the program
-- if its nothing
checkArgsError :: Maybe Runs -> IO Runs
checkArgsError (Just x) = return x
checkArgsError Nothing  = do
  _ <- putStrLn "invalid arguments given"
  _ <- exitWith $ ExitFailure 1
  return []

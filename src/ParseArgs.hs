module ParseArgs (parseArgs, Runs) where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Exit (exitWith, ExitCode(ExitFailure))

type Runs = [Int]

parseArgs :: IO Runs
parseArgs = do
  args <- getArgs
  checkArgsError $ parseRunType args


parseRunType :: [String] -> Maybe Runs
parseRunType [] = return [1..25]
parseRunType (flag : xs)
  | flag `elem` ["--days", "-d"] = ints
  | otherwise = Nothing
  where
    ints = mapM readMaybe xs


checkArgsError :: Maybe Runs -> IO Runs
checkArgsError (Just x) = return x
checkArgsError Nothing  = do
  _ <- putStrLn "invalid arguments given"
  _ <- exitWith $ ExitFailure 1
  return []

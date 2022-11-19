module Main (main) where

import Interface
import Challenges
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.List (intercalate)
import Control.Monad (foldM)

main :: IO ()
main = do
  printTitle "Advent of Code 2022"
  parseArgs >>= printResults . run

printTitle :: String -> IO ()
printTitle title = putStrLn $ "\n" ++ header ++ "\n"
  where
    t = "# " ++ title ++ " #"
    padding = replicate (length t) '#'
    header = intercalate "\n" [padding, t, padding]

printResults :: [Result] -> IO ()
printResults = foldM (const (putStrLn . show)) ()

run :: RunType -> [Result]
run RunAllDays = runDays [1..25]
run (RunDays days) = runDays days

data RunType = RunAllDays | RunDays [Int]
  deriving (Show, Eq)

parseArgs :: IO RunType
parseArgs = do
  args <- getArgs
  case parseRunType args of
    Nothing -> argsError
    Just runType -> return runType


parseRunType :: [String] -> Maybe RunType
parseRunType [] = Just $ RunAllDays
parseRunType (flag : xs)
  | flag `elem` ["--days", "-d"] = ints >>= (Just . RunDays)
  | otherwise = Nothing
  where
    ints = mapM readMaybe xs


argsError :: IO RunType
argsError = do
  _ <- putStrLn "invalid arguments given"
  _ <- exitWith $ ExitFailure 1
  return RunAllDays

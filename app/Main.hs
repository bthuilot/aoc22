{-

Main.hs - the entry point for the advent of code 2022 binary

Copyright (C) 2022  Bryce Thuilot

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
 
-}

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

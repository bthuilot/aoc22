module Main (main) where

import Interface ( Result )
import Challenges ( buildDay, runDay )
import ParseArgs ( parseArgs )
import Data.List (intercalate)
import Control.Monad (foldM)
import Data.Functor ( (<&>) )


-- | 'main' is the entry point for AOC 22
main :: IO ()
main = do
  printTitle "Advent of Code 2022"
  days <- parseArgs <&> map buildDay
  results <- mapM runDay days
  printResults results

-- | 'printTitle' prints the given string as a title to STDOUT
printTitle :: String -> IO ()
printTitle title = putStrLn $ "\n" ++ header ++ "\n"
  where
    t = "# " ++ title ++ " #"
    padding = replicate (length t) '#'
    header = intercalate "\n" [padding, t, padding]

-- | 'printResults' will print all the results given
printResults :: [Result] -> IO ()
printResults = foldM (const print) ()



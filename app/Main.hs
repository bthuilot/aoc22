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

import Interface ( Result )
import Challenges ( buildDay, getInput, runDay )

import ParseArgs ( parseArgs )

import Data.List (intercalate)
import Control.Monad (foldM)


-- | 'main' is the entry point for AOC 22
main :: IO ()
main = do
  printTitle "Advent of Code 2022"
  days <- parseArgs >>= mapM (buildDay getInput)
  printResults $ map runDay days

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



{-|
Module      : Day07
Description : Advent of Code 2022, Day 7 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/7
-}
module Day07 ( day07 ) where

import Interface ( DayRunner )
import GHC.IO.Handle (hGetContents)
import Data.List ( isPrefixOf, sort )
import Utils.Lists ( splitOn, updateFirstWhere )

day07 :: DayRunner
day07 h = do
  contents <- hGetContents h
  let (total , sizes) =  getSizes $ parseInput contents
  return $ map ($ sort sizes) [
    show . sum . takeWhile (< 100000),
    show . head . dropWhile (< (total - 40000000))
    ]

-- | 'Size' is the size of the file
type Size = Int

-- | 'Name' is the name of a file or directory
type Name = String

-- | 'FSDir' is a directory on the file system
data FSDir = Dir Name [FSFile] [FSDir]

-- | 'FSFile' is a file on the filesystem
data FSFile = File Name Size

-- | 'Command' is a command that was ran on the CLI
data Command
  = CD Name
  | LS [LSEntry]

-- | 'LSEntry' is an entry from running "ls"
type LSEntry = String

parseInput :: String -> FSDir
parseInput s = fs
  where
    (fs, _ ) = parseFileSystem (Dir "/" [] []) commands
    commands = drop 1 $ parseCommands (lines s)

parseEntries :: [LSEntry] -> ([FSFile], [FSDir])
parseEntries [] = ([],[])
parseEntries (e : es)
  | "dir " `isPrefixOf` e = (fs, Dir (drop 4 e) [] [] : ds)
  | otherwise = let (s, n) = splitOn ' ' e in (File n (read s) : fs, ds)
  where
    (fs, ds) = parseEntries es

parseFileSystem :: FSDir -> [Command] -> (FSDir, [Command])
parseFileSystem d [] = (d, [])
parseFileSystem d ((CD "..") : xs) = (d, xs)
parseFileSystem (Dir n f ds) ((CD subDir) : xs) = parseFileSystem (Dir n f newSubDirs) remain
  where
    (newSubDir, remain) = parseFileSystem (Dir subDir [] []) xs
    newSubDirs = updateFirstWhere (\(Dir name _ _) -> subDir == name) (const newSubDir) ds
parseFileSystem (Dir n _ _) ((LS ls) : xs) = parseFileSystem (Dir n files dirs) xs
  where
    (files, dirs) = parseEntries ls

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands (x : xs)
  | "$ cd " `isPrefixOf` x = CD (drop 5 x) : parseCommands xs
  | otherwise = LS lsEntries : parseCommands nextCommand
    where
      isNotCommand = not . isPrefixOf "$"
      (lsEntries, nextCommand) = span isNotCommand xs


-- | 'getSizes' will return the a tuple of the total size of of 'FSDir' and a list of
-- all sub directory sizes
getSizes :: FSDir -> (Int, [Int])
getSizes (Dir _ files sub) = (dirTotal, dirTotal : subSizes)
  where
    fileSizes = sum $ map (\(File _ s) -> s) files
    dirTotal = fileSizes + subTotal
    (subTotal, subSizes) = foldl (\(t, acc) d -> let (total, sizes) = getSizes d in (t + total, sizes ++ acc)) (0, []) sub


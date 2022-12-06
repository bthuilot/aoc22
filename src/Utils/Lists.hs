module Utils.Lists where

import Data.List ( isPrefixOf )

dropAndGroup :: (Show a) => (a -> Bool) -> [a] -> [[a]]
dropAndGroup _ [] = []
dropAndGroup p (x : xs)= (x : ys) : rest
  where
    (ys, zs) = span p xs
    rest = dropAndGroup p (drop 1 zs)

-- | 'chunks' will chunk a list into groups of the given size
chunks :: Int -> [a] -> [[a]]
chunks  _ [] = []
chunks i l = take 3 l : chunks i (drop 3 l)


splitBy :: Eq a => [a] -> [a] -> ([a], [a])
splitBy _ [] = ([], [])
splitBy p l@(x : xs)
  | p `isPrefixOf` l = ([], drop (length p) l)
  | otherwise = let (pre,suf) = splitBy p xs in (x : pre, suf)


splitOnAll :: Eq a => [a] -> [a] -> [[a]]
splitOnAll _ [] = []
splitOnAll p xs = pre : splitOnAll p suf
  where
    (pre, suf) = splitBy p xs
 
splitOn :: (Eq a) => a -> [a] -> ([a], [a])
splitOn x xs = (first, drop 1 second)
  where
    (first, second) = span (/= x) xs

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery i l = take (i - 1) l ++ dropEvery i (drop i l)
  

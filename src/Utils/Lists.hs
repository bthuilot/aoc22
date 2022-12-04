module Utils.Lists where


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
 

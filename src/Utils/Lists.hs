module Utils.Lists where


dropAndGroup :: (Show a) => (a -> Bool) -> [a] -> [[a]]
dropAndGroup _ [] = []
dropAndGroup p (x : xs)= (x : ys) : rest
  where
    (ys, zs) = span p xs
    rest = dropAndGroup p (drop 1 zs)

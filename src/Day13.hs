{-|
Module      : Day13
Description : Advent of Code 2022, Day 13 implementation
Copyright   : (c) Bryce Thuilot, 2022
License     : GPL-3
Maintainer  : bryce@thuilot.io

https://adventofcode.com/2022/day/13
-}

{-# LANGUAGE OverloadedStrings #-}

module Day13 ( day13 ) where

import Interface ( DayRunner )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Aeson ( decode, Value(..))
import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
import Data.List (sortBy)


type Packet = Value

day13 :: DayRunner
day13 h = do
  contents <- B.hGetContents h
  let vals = parsePackets contents
  return [
    show $ sortPairs vals,
    show $ fst $ foldl (\(acc, ps) p ->
                           let
                             (i, ps') = insertPacket p ps
                           in (acc * i, ps')
                       ) (1, sortBy compareData vals) dividerPackets
    ]


dividerPackets :: [Value]
dividerPackets = [
  Array (V.singleton $ Array (V.singleton $ Number 2)),
  Array (V.singleton $ Array (V.singleton $ Number 6))
  ]

sortPairs :: [Value] -> Int
sortPairs = sortPairs' (1, 0)
  where
    sortPairs' (i, acc) (p1 : p2 : xs) = sortPairs' (i + 1, acc + if compareData p1 p2 == LT then i else 0) xs
    sortPairs' (_, acc) _ = acc

parsePackets :: B.ByteString -> [Value]
parsePackets = map parsePacket . filter (not . B.null) . B.split 10 


insertPacket :: Packet -> [Packet] -> (Int, [Packet])
insertPacket = insertElement' 1
  where
    insertElement' i v [] = (i, [v])
    insertElement' i v ps@(p : ps')
      = case compareData v p of
          GT -> let (i', l) = insertElement' (i + 1) v ps' in (i', p : l)
          _ -> (i, v : ps)

parsePacket :: B.ByteString -> Value
parsePacket = fromMaybe Null . decode . LB.fromStrict

compareData :: Packet -> Packet -> Ordering
compareData (Number l) (Number r) = compare l  r
compareData (Array l) (Array r) = compareItems l r
compareData (Array l) r@(Number _) = compareItems l (V.singleton r)
compareData l@(Number _) (Array r) = compareItems (V.singleton l) r
compareData _ _ = EQ



compareItems :: V.Vector Value -> V.Vector Value -> Ordering
compareItems l r
  | V.null l && V.null r = EQ
  | V.null l = LT
  | V.null r = GT
  | order == EQ = compareItems (V.tail l) (V.tail r)
  | otherwise = order
    where
      (lHead, rHead) = (V.head l, V.head r)
      order = compareData lHead rHead
      

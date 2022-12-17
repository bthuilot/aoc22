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


type PacketPair = (Value, Value)


day13 :: DayRunner
day13 h = do
  contents <- B.hGetContents h
  let vals = parseContents contents
  return [show $ snd $ foldl (\(i, acc) p -> (i + 1, if compareData p == LT then i+acc else acc)) (1, 0) vals]


splitOnSubstring :: B.ByteString -> B.ByteString -> [B.ByteString]
splitOnSubstring sub s
  | B.null end = [start]
  | otherwise = start :  splitOnSubstring sub end
  where
    (start, end') = B.breakSubstring sub s
    end = B.drop 2 end'

parseContents :: B.ByteString -> [PacketPair]
parseContents = map parsePair . splitOnSubstring "\n\n"
  where
    parsePair s = let (l, r) = B.breakSubstring "\n" s in (parsePacket l, parsePacket r)


parsePacket :: B.ByteString -> Value
parsePacket = fromMaybe Null . decode . LB.fromStrict



compareData :: PacketPair -> Ordering
compareData (Number l, Number r) = compare l  r
compareData (Array l, r@(Number _)) = compareItems l (V.singleton r)
compareData (l@(Number _), Array r) = compareItems (V.singleton l) r
compareData (Array l, Array r) = compareItems l r
compareData _ = EQ



compareItems :: V.Vector Value -> V.Vector Value -> Ordering
compareItems l r
  | V.null l && V.null r = EQ
  | V.null l = LT
  | V.null r = GT
  | order == EQ = compareItems (V.tail l) (V.tail r)
  | otherwise = order
    where
      (lHead, rHead) = (V.head l, V.head r)
      order = compareData (lHead, rHead)
      

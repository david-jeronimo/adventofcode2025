{-# LANGUAGE OverloadedStrings #-}

module Aoc02 (parseInput, solution) where

import Data.List (nub)
import qualified Data.Text as T
import Lib (Part (..), Text)

parseInput :: Text -> [(Int, Int)]
parseInput txt = [(readInt a, readInt b) | [a, b] <- map (T.splitOn "-") . T.splitOn "," $ txt]
  where
    readInt = read . T.unpack

solution :: Part -> Bool -> [(Int, Int)] -> Int
solution part _ = sum . concatMap (invalidIds part)

invalidIds :: Part -> (Int, Int) -> [Int]
invalidIds part (from, to) = filter (isInvalid part) [from .. to]

isInvalid :: Part -> Int -> Bool
isInvalid part n = any invalid . segmentSizes part . T.length $ textN
  where
    textN = T.show n
    invalid size = (== 1) . length . take 2 . nub . T.chunksOf size $ textN

segmentSizes :: Part -> Int -> [Int]
segmentSizes part n = case part of
  PartOne -> [n `div` 2 | even n]
  PartTwo -> [d | d <- [1 .. pred n], n `rem` d == 0]

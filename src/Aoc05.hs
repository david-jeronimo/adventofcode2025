{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc05 (parseInput, solution) where

import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Tuple.Extra (both)
import Lib (Part (..), Text, toTuple2)

data Input = Input {ranges :: [(Int, Int)], ingredients :: [Int]} deriving (Show)

parseInput :: Text -> Input
parseInput = uncurry Input . bimap parseRange (map readInt . T.lines) . splitBlocks
  where
    splitBlocks = fromJust . toTuple2 . T.splitOn "\n\n"
    readInt = read . T.unpack
    parseRange = map (both readInt . fromJust . toTuple2 . T.splitOn "-") . T.lines

solution :: Part -> Bool -> Input -> Int
solution part _ Input {..} = case part of
  PartOne -> length $ filter (isFresh ranges) ingredients
  PartTwo -> sum . map numIngredients . mergeRanges $ ranges
    where
      numIngredients (a, b) = b - a + 1

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges ingredient = any inRange ranges
  where
    inRange (a, b) = a <= ingredient && b >= ingredient

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = foldl addRange [] . sort

addRange :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
addRange ranges@((prevA, prevB) : rest) (a, b)
  | a > prevB + 1 = (a, b) : ranges
  | b < prevB = ranges
  | otherwise = (prevA, b) : rest
addRange [] r = [r]
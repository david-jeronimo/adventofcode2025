{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Aoc03 (parseInput, solution) where

import Data.List (singleton, tails)
import Data.List.Extra (groupSortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Lib (Part (..), Text)

parseInput :: Text -> [[Int]]
parseInput = map (map (read . singleton) . T.unpack) . T.lines

solution :: Part -> Bool -> [[Int]] -> Int
solution part _ = sum . map (joltage size 0)
  where
    size = case part of
      PartOne -> 2
      PartTwo -> 12

joltage :: Int -> Int -> [Int] -> Int
joltage 0 acc _ = acc
joltage size acc segment = maximum . map maxJoltage $ remSegments
  where
    subSegments = [s | s <- tails segment, length s >= size]
    remSegments = head . groupSortOn (Down . head) $ subSegments
    maxJoltage (x : xs) = joltage (pred size) (acc + (x * 10 ^ pred size)) xs
    maxJoltage _ = 0
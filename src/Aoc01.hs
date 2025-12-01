{-# LANGUAGE RecordWildCards #-}

module Aoc01 (parseInput, solution) where

import Data.List.Split (divvy)
import qualified Data.Text as T
import Lib (Part (..), Text)

data Dir = L | R deriving Read

data Rotation = Rotation {dir :: Dir, times :: Int}

parseInput :: Text -> [Rotation]
parseInput txt = [Rotation (read [d]) (read t) | (d : t) <- map T.unpack . T.lines $ txt]

solution :: Part -> Bool -> [Rotation] -> Int
solution part _ rotations =
  sum
    [ numZeroes part (a, b)
    | [a, b] <- divvy 2 1 . scanl rotate 50 $ rotations
    ]

rotate :: Int -> Rotation -> Int
rotate n Rotation {..} = case dir of
  L -> n - times
  R -> n + times

numZeroes :: Part -> (Int, Int) -> Int
numZeroes part (a, b) = case part of
  PartOne | isZero b -> 1
  PartTwo
    | isZero b && a > b -> succ distance
    | isZero a && a > b -> pred distance
    | otherwise -> distance
  _ -> 0
  where
    isZero n = n `rem` 100 == 0
    distance = abs (segment a - segment b)
    segment = (`div` 100)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc06 (parseInput, solution) where

import Data.Bifunctor (bimap)
import Data.List (unsnoc)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Lib (Part (..), Text)

type Op = Int -> Int -> Int

data Input = Input {numbers :: [[Text]], operators :: [Op]}

parseInput :: Text -> Input
parseInput = uncurry Input . bimap parseNumbers parseOperators . fromJust . unsnoc . T.lines
  where
    parseOperators = map (parseOp . T.head) . T.words
    parseOp '+' = (+)
    parseOp _ = (*)
    parseNumbers = splitWhen (T.all (== ' ')) . T.transpose

solution :: Part -> Bool -> Input -> Int
solution part _ Input {..} = sum . zipWith (calcCol part) operators $ numbers

calcCol :: Part -> Op -> [Text] -> Int
calcCol part op = foldl1 op . getNumbers part

getNumbers :: Part -> [Text] -> [Int]
getNumbers = \case
  PartOne -> parseNumbers . T.transpose
  PartTwo -> parseNumbers
  where
    parseNumbers = map (read . T.unpack)

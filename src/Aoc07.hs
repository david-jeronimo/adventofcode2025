{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Aoc07 (parseInput, solution) where

import Data.Bifunctor (bimap, first)
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, intersection, member, size, toList)
import qualified Data.Text as T
import Lib (Part (..), Text)

type Pos = (Int, Int)

data Input = Input {start :: Pos, splitters :: Set Pos}

data Dir = L | R | D

parseInput :: Text -> Input
parseInput = uncurry Input . bimap startPos splitterPos . fromJust . uncons . T.lines
  where
    startPos = (0,) . fromJust . T.findIndex (== 'S')
    splitterPos lines' =
      fromList
        [ (j, i)
        | (j, line) <- zip [1 ..] lines',
          (i, '^') <- zip [0 ..] . T.unpack $ line
        ]

solution :: Part -> Bool -> Input -> Int
solution part _ Input {..} = case part of
  PartOne -> size $ intersection splitters visitedPositions
  PartTwo -> M.foldr (+) 0 . (!! maxY) $ iterations
  where
    maxY = maximum . map fst . toList $ splitters
    visitedPositions = fromList . map (move D) . concatMap M.keys . take maxY $ iterations
    iterations = iterate (moveBeams splitters) $ M.singleton start 1

moveBeams :: Set Pos -> Map Pos Int -> Map Pos Int
moveBeams splitters = M.fromListWith (+) . concatMap (nextPos splitters) . M.assocs

nextPos :: Set Pos -> (Pos, Int) -> [(Pos, Int)]
nextPos splitters (pos, n) = (,n) <$> positions
  where
    positions
      | move D pos `member` splitters = liftA2 move [L, R] $ pure pos
      | otherwise = pure . move D $ pos

move :: Dir -> Pos -> Pos
move dir (y, x) = (succ y,) $ case dir of
  L -> pred x
  R -> succ x
  D -> x

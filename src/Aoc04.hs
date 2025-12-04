module Aoc04 (parseInput, solution) where

import Data.List (unfoldr)
import Data.Set (Set, fromList, intersection, size, (\\))
import qualified Data.Set as S
import qualified Data.Text as T
import Lib (Part (..), Text)

type Pos = (Int, Int)

parseInput :: Text -> Set Pos
parseInput txt =
  fromList
    [ (j, i)
    | (j, line) <- zip [0 ..] . T.lines $ txt,
      (i, '@') <- zip [0 ..] . T.unpack $ line
    ]

solution :: Part -> Bool -> Set Pos -> Int
solution part _ = case part of
  PartOne -> size . accessibleRolls
  PartTwo -> sum . unfoldr removeRolls

accessibleRolls :: Set Pos -> Set Pos
accessibleRolls rolls = S.filter (isAccessible rolls) rolls

removeRolls :: Set Pos -> Maybe (Int, Set Pos)
removeRolls rolls = case accessibleRolls rolls of
  toRemove | null toRemove -> Nothing
  toRemove -> Just (size toRemove, rolls \\ toRemove)

isAccessible :: Set Pos -> Pos -> Bool
isAccessible rolls = (< 4) . size . intersection rolls . adj8

adj8 :: Pos -> Set Pos
adj8 pos@(y, x) = fromList . filter (/= pos) $ liftA2 (,) [pred y .. succ y] [pred x .. succ x]

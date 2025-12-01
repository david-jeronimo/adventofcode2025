{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( Part (..),
    Text,
    readInput,
  )
where

import Data.Text (Text)
import qualified Data.Text.IO as TIO

data Part = PartOne | PartTwo deriving (Show, Eq, Enum)

readInput :: (Text -> a) -> String -> IO a
readInput parse file = do
  l <- TIO.readFile file
  let input = parse l
  return input
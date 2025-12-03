module LibLoad (runModule) where

import Lib (Part, Text)
import qualified Aoc01
import qualified Aoc02
import qualified Aoc03
-- import qualified Aoc04
-- import qualified Aoc05
-- import qualified Aoc06
-- import qualified Aoc07
-- import qualified Aoc08
-- import qualified Aoc09
-- import qualified Aoc10
-- import qualified Aoc11
-- import qualified Aoc12

runModule :: String -> Part -> Bool -> Text -> IO ()
runModule day p s = case day of 
          "01" -> print . Aoc01.solution p s . Aoc01.parseInput
          "02" -> print . Aoc02.solution p s . Aoc02.parseInput
          "03" -> print . Aoc03.solution p s . Aoc03.parseInput
        --   "04" -> print . Aoc04.solution p s . Aoc04.parseInput
        --   "05" -> print . Aoc05.solution p s . Aoc05.parseInput
        --   "06" -> print . Aoc06.solution p s . Aoc06.parseInput
        --   "07" -> print . Aoc07.solution p s . Aoc07.parseInput
        --   "08" -> print . Aoc08.solution p s . Aoc08.parseInput
        --   "09" -> print . Aoc09.solution p s . Aoc09.parseInput
        --   "10" -> print . Aoc10.solution p s . Aoc10.parseInput
          -- "11" -> print . Aoc11.solution p s . Aoc11.parseInput
          -- "12" -> print . Aoc12.solution p s . Aoc12.parseInput
          _    -> const . print $ "Wrong day"
import Lib (Part(..))
import LibLoad (runModule)
import System.TimeIt (timeItT)
import Text.Printf
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Tuple.Extra ((&&&))
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)


main :: IO ()
main = do
    [day] <- getArgs
    timeItNamed' "Total" $ runDay day
runDay:: String -> IO()
runDay dayStr = sequence_ $ liftA2 (run dayStr) (fileSample <$> ["sample","input"]) [PartOne,PartTwo]
  where fileName suffix = "files/aoc" <> dayStr <> "." <> suffix <> ".txt"
        fileSample = fileName &&& (== "sample")

run :: String -> (FilePath, Bool) -> Part -> IO()
run day (file, isSample) part = timeItNamed' "CPU time" $ do   
    putStrLn $ "\n" <> show part <> "  " <> file
    l <- TIO.readFile file
    runModule day part isSample l

timeItNamed' :: MonadIO m => String -> m a -> m a
timeItNamed' name ioa = do
    (t, a) <- timeItT ioa
    liftIO $ printf (name ++ ": %6.3fs\n") t
    return a
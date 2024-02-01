module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Applicative ((<|>))
import Control.Monad (forever, (>=>), forM_)
import Control.Concurrent.Timer (repeatedTimer, repeatedStart, newTimer)
import Control.Concurrent.Suspend
import Positions
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL8
import Data.Function ((&))
import qualified Data.Vector as V
import Data.Bifunctor (second)
import Data.Set ((\\), toList)


main :: IO ()
main = do
    timer <- newTimer
    started <- repeatedStart timer computation (sDelay 4)
    forever $ threadDelay maxBound -- keep main thread alive

parseToPositionCollection :: Traversable t => t (Source, BL8.ByteString) -> Either String (t PositionCollection)
parseToPositionCollection listOfData = do
    let toPositions' = toPositions . BL8.toString
    let listOfPositions = fmap (second toPositions') listOfData
    traverse (uncurry toPositionCollection) listOfPositions


compareSources :: PositionCollection -> PositionCollection -> IO ()
compareSources (PositionCollection name1 pos1) (PositionCollection name2 pos2) =
    let _in1not2 = pos1 \\ pos2 & toList & fmap (\pos -> show pos ++ "\n")
        _in2not1 = pos2 \\ pos1 & toList & fmap (\pos -> show pos ++ "\n")
    in putStrLn ("positions in " ++ fileName name1 ++ " not in " ++ fileName name2 ++ "\n")
    >> forM_ _in1not2 putStrLn
    >> putStrLn ("positions in " ++ fileName name2 ++ " not in " ++ fileName name1 ++ "\n")
    >> forM_ _in2not1 putStrLn

computation :: IO ()
computation = do
    let (file1, file2) = (File "file1", File "file2")
    seedPositionFile file1
    seedPositionFile file2
    content <- BL.readFile $ fileName file1
    content2 <- BL.readFile $ fileName file2
    case parseToPositionCollection [(File "file1", content), (File "file2", content2)] of
        Left error -> putStrLn error
        Right positionLists -> do
            let pairs = [(x, y) | x <- positionLists, y <- positionLists, x /= y]
            forM_ pairs (uncurry compareSources)

pos = undefined :: Position

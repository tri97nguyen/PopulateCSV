{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
module Positions where
import System.Random
import Data.Function ( (&) )
import Control.Monad ( join, forM, (>=>) )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL8
import Data.Csv
import qualified Data.Vector as V
import Data.ByteString.UTF8 (fromString)
import Data.Set (fromList, Set)
import Data.Foldable (toList)


data Source = File {fileName :: String} | Database String deriving (Eq, Show)
readAndPrintPositions :: IO ()
readAndPrintPositions = do
    content <- BL.readFile "test.csv"
    case content & BL8.toString & toPositions of
        Left err -> putStrLn "fail to parse positions file"
        Right positions -> undefined--printPositions positions

data Position = Position {fundstudioId :: !String, account :: !String, systemCode :: !String, position :: !Double} deriving (Show, Eq, Ord)
data PositionCollection = PositionCollection {collectionName :: Source, positions :: Set Position} deriving Eq

instance FromNamedRecord Position where -- this is just to use the csv libary
    parseNamedRecord r = Position <$> r .: "FundstudioId" <*> r .: "Account" <*> r .: "SystemCode" <*> r .: "Position"

toPositions :: String -> Either String (Set Position)
toPositions rawString =
    let content = BL8.fromString rawString
    in case decodeByName content of
        Left err -> Left err
        Right (header,v) -> Right (fromList . toList $ v)

toPositionCollection :: Source -> Either a (Set Position) -> Either a PositionCollection
toPositionCollection metadata positions =
    case positions of
        Left error -> Left error
        Right pos -> Right $ PositionCollection {collectionName = metadata, positions = pos}


toPositions' rawString = fmap snd (decodeByName $ BL8.fromString rawString)

printPositions :: V.Vector Position -> IO ()
printPositions positions = V.forM_ positions print
printPositions' :: V.Vector Position -> IO ()
printPositions' = V.mapM_ print


seedPositionFile :: Source -> IO ()
seedPositionFile (File filename) = do
    myData <- mconcat $ [1..5] & fmap createLine
    let content = csvHeader ++ myData
    writeFile filename content

createLine :: Int -> IO String
createLine securityId = do
    let systemCodes = ["STX", "DEQ", "AB5", "FTX", "GET", "GES"]
    let account = ["A123", "A124", "B12X", "C52Z", "AB1", "CQX"]
    sysCodeIndex <- randomRIO (0, length systemCodes - 1) :: IO Int
    accountIndex <- randomRIO (0, length account - 1) :: IO Int
    randomPosition <- randomRIO (1, 200) :: IO Int
    return (show securityId ++ "," ++ (account !! accountIndex) ++ "," ++ (systemCodes !! sysCodeIndex) ++ "," ++ show randomPosition ++ "\n")

csvHeader = "FundstudioId,Account,SystemCode,Position\n"
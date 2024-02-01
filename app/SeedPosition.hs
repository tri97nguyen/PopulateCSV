module SeedPosition where
    import System.Random
    import Data.Function ( (&) )
    import Control.Monad (join)
    
    createPositionFile :: String -> IO ()
    createPositionFile fileName = do
        myData <- mconcat $ [1..2] & map createLine
        let content = header ++ myData 
        writeFile fileName content

    createLine :: Int -> IO String
    createLine securityId = do
        let systemCodes = ["STX", "DEQ", "AB5", "FTX", "GET", "GES"]
        let account = ["A123", "A124", "B12X", "C52Z", "AB1", "CQX"]
        sysCodeIndex <- randomRIO (0, length systemCodes - 1) :: IO Int
        accountIndex <- randomRIO (0, length account - 1) :: IO Int
        randomPosition <- randomRIO (1, 200) :: IO Int
        return (show securityId ++ "," ++ (account !! accountIndex) ++ "," ++ (systemCodes !! sysCodeIndex) ++ "," ++ show randomPosition ++ "\n")

    header = "FundstudioId,Account,SystemCode,Position\n"
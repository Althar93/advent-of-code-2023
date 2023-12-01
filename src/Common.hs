module Common (
    executeAndPrintResults,
    ) where

import Data.Time
import Data.List

-- Convenience function for executing & printing the result of a puzzle
executeAndPrintResults :: (Show a) => String -> IO a -> IO ()
executeAndPrintResults title solver = do
    startTime <- getCurrentTime
    result <- solver
    putStrLn $ title ++ " : " ++ show result
    endTime <- getCurrentTime
    putStrLn $ "Took " ++ (show (diffUTCTime endTime startTime)) ++ " to execute."

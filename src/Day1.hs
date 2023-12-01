module Day1 (day1Solver) where

import Data.Char (isDigit)
import Data.List (sum)
import Data.Text (pack, unpack, replace)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day1_test_input.txt"

-- The test input file path for part two
testInputFilePartTwo :: FilePath
testInputFilePartTwo = "res/day1_test_input_part_two.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day1_input.txt"

-- A calibration value
type CalibrationValue = [Char]

-- Reads the test input
readInputs :: IO [CalibrationValue]
readInputs = do
    contents <- readFile inputFile
    return $ lines contents

-- Converts letter digits contained in a string to its corresponding digit
convertStringToDigit :: String -> String
convertStringToDigit xs = foldr replaceDigit xs [("one", "o1ne"), ("two", "t2wo"), ("three", "thr3ee"), ("four", "fo4ur"), ("five", "fi5ve"), ("six", "s6ix"), ("seven", "se7ven"), ("eight", "ei8ght"), ("nine", "ni9ne")] where
    replaceDigit (a, b) = unpack . replace (pack a) (pack b) . pack 

-- Decodes the calibration value
decodeCalibrationValue :: CalibrationValue -> Int
decodeCalibrationValue xs = read (head ds : [last ds]) :: Int where
    ds = filter isDigit xs

-- The solver for part #1 of the puzzle
solvePart1 :: [CalibrationValue] -> Int
solvePart1 xs = sum $ map decodeCalibrationValue xs

-- The solver for part #2 of the puzzle
solvePart2 :: [CalibrationValue] -> Int
solvePart2 xs = sum $ map decodeCalibrationValue xs' where
    xs' = map convertStringToDigit xs

-- The full solver
day1Solver :: IO [Int]
day1Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

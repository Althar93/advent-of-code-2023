module Day9 (day9Solver) where

import Parser
import Data.List (sum)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day9_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day9_input.txt"

-- A single history of values
type History = [Int]

-- Parses an instruction
parseHistory :: Parser History
parseHistory = do
    parseSpaces
    ns <- some $ parseMore <|> parseInt
    return ns
    where
        parseMore = do
            n <- parseInt
            parseString " "
            return n

-- Parses a network
parseHistories :: Parser [History]
parseHistories = do
    hs <- some $ parseHistory
    return hs

-- Reads the test input
readInputs :: IO [History]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseHistories contents

-- Computes the differences in history
differences :: History -> History
differences (x0:x1:xs) = (x1 - x0) : (differences (x1:xs))
differences (x:[])     = []

-- Steps the differences
stepDifferences :: History -> [History]
stepDifferences xs = xs'' ++ [xs] where
    xs'' = case all (==0) xs' of
             True  -> [0:xs']
             False -> stepDifferences xs'
    xs'  = differences xs

-- Solve the difference
solve :: (Int -> Int -> Int) -> History -> History -> History
solve f x0 x1 = x0 ++ [x] where
    x = f (last x1) (last x0)

-- Steps the solver
stepSolve :: (Int -> Int -> Int) -> [History] -> History
stepSolve f (x1:[x0])  = solve f x0 x1
stepSolve f (x1:x0:xs) = stepSolve f ((solve f x0 x1):xs)

-- Computes the next value in the history
nextHistory :: History -> Int
nextHistory xs = last (stepSolve (\x1 x0 -> x0 + x1) ds) where
    ds = stepDifferences xs

-- Computes the previous value in the history
prevHistory :: History -> Int
prevHistory xs = last (stepSolve (\x1 x0 -> x0 - x1) ds) where
    ds = map reverse (stepDifferences xs)

-- The solver for part #1 of the puzzle
solvePart1 :: [History] -> Int
solvePart1 hs = sum ns where
    ns = map nextHistory hs

-- The solver for part #2 of the puzzle
solvePart2 :: [History] -> Int
solvePart2 hs = sum ns where
    ns = map prevHistory hs

-- The full solver
day9Solver :: IO [Int]
day9Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

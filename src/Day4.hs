module Day4 (day4Solver) where

import Parser
import Data.List (sum)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day4_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day4_input.txt"

-- A card
type Card = (Int, [Int], [Int])

-- Parses a single card
parseCard :: Parser Card
parseCard = do
    parseSpaces
    parseString "Card"
    parseSpaces
    n <- parseInt
    parseString ":"
    ws <- some $ parseNumber
    parseString " |"
    ns <- some $ parseNumber
    return (n, ws, ns) where
        parseNumber = do
            parseSpaces
            n <- parseInt
            return n

-- Parses some cards
parseCards :: Parser [Card]
parseCards = some $ parseCard

-- Reads the test input
readInputs :: IO [Card]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseCards contents

-- Returns the winning number of a card
winningNumbers :: Card -> [Int]
winningNumbers (_, ws, ns) = filter (\n -> elem n ws) ns

-- Computes the score for a given set of winning numbers
scoreNumbers :: [Int] -> Int
scoreNumbers [] = 0
scoreNumbers xs = 2 ^ (length xs - 1)

-- The solver for part #1 of the puzzle
solvePart1 :: [Card] -> Int
solvePart1 xss = sum scores where 
    scores         = map (scoreNumbers . winningNumbers) xss where

-- Returns the winning cards
winningCards :: [Card] -> Card -> Maybe [Card]
winningCards xs x@(i0, _, _) = case winningNumbers x of
                                [] -> Nothing
                                ws -> Just $ map (\i -> xs !! i) [i0 + i' | i' <- [0..length ws - 1]]

-- Naive/brute force implementation of the card game
winCards :: [Card] -> Int
winCards xs = length $ winCards' xs xs where
    winCards' xs []     = []
    winCards' xs (y:ys) = case winningCards xs y of
                            Nothing  -> y : (winCards' xs ys)
                            Just ys' -> y : (winCards' xs (ys'++ys))

-- Fast implementation of the card game
winCardsFast :: [Card] -> Int
winCardsFast xs = sum $ reducedWinningPairs xs

-- Reduces the winning pairs of the given cards
reducedWinningPairs :: [Card] -> [Int]
reducedWinningPairs xs = reduceWinningPairs $ winningPairs xs

-- Reduces the winning pairs to their absolute wins
reduceWinningPairs :: [[Int]] -> [Int]
reduceWinningPairs ws = map (reduceWinningPairs' ws) ws where
    reduceWinningPairs' ws [] = 1
    reduceWinningPairs' ws xs = 1 + (sum $ map (\i -> reduceWinningPairs' ws (ws !! (i - 1))) xs) 


-- Builds a list of winning cards for ONE card
buildWinningPairs :: [Card] -> Card -> [Int]
buildWinningPairs xs x = case winningCards xs x of
                               Nothing -> []
                               Just ys -> map (\(i, _, _) -> i) ys

-- Builds a list of winning cards for each card
winningPairs :: [Card] -> [[Int]]
winningPairs xs = map (buildWinningPairs xs) xs


-- The solver for part #2 of the puzzle
solvePart2 :: [Card] -> Int
solvePart2 xss = winCardsFast xss

-- The full solver
day4Solver :: IO [Int]
day4Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

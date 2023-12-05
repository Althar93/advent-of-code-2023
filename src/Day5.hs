module Day5 (day5Solver) where

import Parser
import Data.Char (isAlphaNum)
import Data.List (sum, find)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day5_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day5_input.txt"

-- Seeds
type Seeds = [Int]

-- Map
data Map = Map String String [(Int, Int, Int)] deriving (Show)

-- Almanac
type Almanac = (Seeds, [Map])

-- Parses seeds
parseSeeds :: Parser Seeds
parseSeeds = do
    parseString "seeds:"
    xs <- some $ parseSeedNumber 
    return xs 
        where
        parseSeedNumber = do
            parseSpaces
            i <- parseInt
            return i

-- Parses a single map
parseMap :: Parser Map
parseMap = do
    parseSpaces
    s <- some $ parseIs isAlphaNum
    parseString "-to-"
    d <- some $ parseIs isAlphaNum
    parseString " map:"
    xs <- some $ parseSeedRange 
    return $ Map s d xs
        where
        parseSeedRange = do
            parseSpaces 
            i <- parseInt
            parseSpaces
            j <- parseInt
            parseSpaces
            k <- parseInt
            return (i, j, k)

-- Parses the almanac
parseAlmanac :: Parser Almanac
parseAlmanac = do
    ss <- parseSeeds
    parseSpaces
    ms <- some $ parseMap
    return (ss, ms)

-- Reads the test input
readInputs :: IO Almanac
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseAlmanac contents

-- Remaps a single seed
remapSeed :: [(Int, Int, Int)] -> Int -> Int
remapSeed xs x = case find (\(_, s, r) -> x >= s && x < s + r) xs of
                   Nothing        -> x
                   Just (e, s, _) -> (x - s) + e

-- Locates a single seed
locateSeed :: [Map] -> Int -> Int
locateSeed [] x                = x
locateSeed ((Map _ _ bs):xs) x = locateSeed xs (remapSeed bs x) 

-- BRUTE FORCE : Expands the seeds based on pairs of values
expandSeeds :: [Int] -> [Int]
expandSeeds []       = []
expandSeeds (x:r:xs) = [x + i | i <- [0..r]] ++ (expandSeeds xs)

-- The solver for part #1 of the puzzle
solvePart1 :: Almanac -> Int
solvePart1 xss = minimum locations where
    locations = map (locateSeed (snd xss)) (fst xss) 

-- The solver for part #2 of the puzzle
solvePart2 :: Almanac -> Int
solvePart2 xss = minimum locations where
    locations = map (locateSeed (snd xss)) (expandSeeds (fst xss))

-- The full solver
day5Solver :: IO [Int]
day5Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

module Day12 (day12Solver) where

import Parser
import Data.List (group, intersperse)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day12_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day12_input.txt"

-- A single record of springs' conditions
type Record = (String, [Int])

-- Parses a single record
parseRecord :: Parser Record
parseRecord = do
    parseSpaces
    xs <- some $ parseIsOneOf "?#."
    parseSpaces
    ns <- some $ parseMore <|> parseInt
    return (xs, ns) 
    where
        parseMore = do
            parseChar ','
            n <- parseInt
            return n

-- Parses some records
parseRecords :: Parser [Record]
parseRecords = some parseRecord

-- Reads the test input
readInputs :: IO [Record]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseRecords contents

-- Returns whether an arrangement is valid
isValidArrangement :: [Int] -> String -> Bool
isValidArrangement na xb = na == (makeArrangement xb)

-- Returns the corresponding arrangement to the completed record
makeArrangement :: String -> [Int]
makeArrangement xs = map length (filter (\xs -> (head xs) == '#') (group xs))

unknowns :: String -> [Int]
unknowns xs = map length (filter (\xs -> (head xs) == '?') (group xs))

-- Replaces all unknowns
replaceUnknowns :: String -> String -> String
replaceUnknowns xs []                     = xs
replaceUnknowns (x:xs) (y:ys) | x == '?'  = y : (replaceUnknowns xs ys)
                              | otherwise = x : (replaceUnknowns xs (y:ys))

-- (BFS) Returns all possible arrangements that fulfill the record
arrangements :: Record -> [String]
arrangements (xs, ns) = buildArrangements nD nO (xs, ns) "" where
    nD = (sum ns) - (sum (makeArrangement xs))
    nO = (sum (unknowns xs)) - nD

-- Builds list of arrangements in a BFS fashion
buildArrangements :: Int -> Int -> Record -> String -> [String]
buildArrangements 0 0 (xs, ns) ys = case (isValidArrangement ns) (replaceUnknowns xs ys) of
                                        True  -> [ys]
                                        False -> []
buildArrangements nD nO r ys     = left ++ right where
    left  = if nD > 0 then (buildArrangements (nD - 1) nO r ('#':ys)) else []
    right = if nO > 0 then (buildArrangements nD (nO - 1) r ('.':ys)) else []

-- Unfolds the record 
unfold :: Record -> Record
unfold (xs, ns) = (concat $ intersperse "?" (take 5 (repeat xs)), concat $ (take 5 (repeat ns)))

-- The solver for part #1 of the puzzle
solvePart1 :: [Record] -> Int
solvePart1 hs = sum $ map (length . arrangements) hs

-- The solver for part #2 of the puzzle
solvePart2 :: [Record] -> Int
solvePart2 hs = sum $ map (length . arrangements) hs' where
    hs' = map unfold hs

-- The full solver
day12Solver :: IO [Int]
day12Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

module Day3 (day3Solver) where

import Data.List (sum)
import Data.Char (isDigit, digitToInt)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day3_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day3_input.txt"

-- A schematic
type Schematic = [[Char]]

-- Reads the test input
readInputs :: IO Schematic
readInputs = do
    contents <- readFile inputFile
    return $ lines contents

-- Returns the numbers within the schematic
numbers :: Schematic -> [(Int, Int, Int, Int)]
numbers xss = numbers' 0 [] xss where
    numbers' _ acc []       = acc
    numbers' y acc (xs:xss) = (numbersRow y xs) ++ (numbers' (y + 1) acc xss) 

-- Returns the numbers within a row with the value, start and end indices
numbersRow :: Int -> [Char] -> [(Int, Int, Int, Int)]
numbersRow y xs = numbersRow' y 0 [] xs where
    numbersRow' _ _ acc []     = reverse acc
    numbersRow' j i acc (x:xs) = case isDigit x of
                                 True -> case acc of
                                           ((n, _, s, e):acc') -> if e == i - 1
                                                                  then numbersRow' y (i + 1) ((n * 10 + (digitToInt x), y, s, i):acc') xs
                                                                  else numbersRow' y (i + 1) (((digitToInt x), y, i, i):(n, y, s, e):acc') xs
                                           []               -> numbersRow' y (i + 1) [((digitToInt x), y, i, i)] xs
                                 False -> numbersRow' y (i + 1) acc xs

-- Returns all cells within the vicinity of the specified range
neighbours :: Schematic -> (Int, Int, Int) -> [Char]
neighbours xss (y0, x0, x1) = [((xss !! y) !! x) | x <- [xMin..xMax], y <- [yMin..yMax]] where
    xMin = max (x0 - 1) 0
    xMax = min (x1 + 1) (length (xss !! 0) - 1) 
    yMin = max (y0 - 1) 0
    yMax = min (y0 + 1) (length xss - 1)

-- Returns whether a character is considered a symbol
isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol c   = not $ isDigit c

-- Returns whether a symbol is a gear
isGear :: Char -> Bool
isGear '*' = True
isGear _   = False

-- Returns all part numbers in the schematic
partNumbers :: Schematic -> [Int]
partNumbers xss = map (\(n, _, _, _) -> n) (partNumbers' xss) where
    partNumbers' xss               = filter (isValidPart xss) (numbers xss)
    isValidPart xss (n, y, x0, x1) = any isSymbol (neighbours xss (y, x0, x1))
 
-- Finds the specific symbols
findSymbols :: (Char -> Bool) -> Schematic -> [(Int, Int)]
findSymbols p xss = [(x, y) | x <- [0..length (xss !! 0) - 1], y <- [0..length xss - 1], (isGear ((xss !! y) !! x))]

-- Finds the part numbers adjacent to a location
findPartNumbers :: Schematic -> (Int, Int) -> [Int]
findPartNumbers xss (x0, y0) = map (\(n, _, _, _) -> n) adjacentNumbers where
    adjacentNumbers = filter (\(_, y, s, e) -> abs(y0 - y) <= 1 && x0 <= e + 1 && x0 >= s - 1) parts
    parts = numbers xss 

-- The solver for part #1 of the puzzle
solvePart1 :: Schematic -> Int
solvePart1 xss = sum (partNumbers xss)

-- The solver for part #2 of the puzzle
solvePart2 :: Schematic -> Int
solvePart2 xss = sum gearRatios where
    gearRatios = map product gearPairs'
    gearPairs' = filter (\xs -> length xs > 1) gearPairs
    gearPairs  = map (findPartNumbers xss) gears
    gears      = findSymbols isGear xss 

-- The full solver
day3Solver :: IO [Int]
day3Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

module Day13 (day13Solver) where

import Parser

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day13_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day13_input.txt"

-- A single mirror
type Mirror = [[Char]]

-- Parses a single mirror
parseMirror :: Parser Mirror
parseMirror = do
    parseSpaces
    xss <- some parseMirrorRow
    return xss
    where
        parseMirrorRow = do
            xs <- some $ parseIsOneOf ".#"
            parseLineReturn
            return xs

-- Parses some mirrors
parseMirrors :: Parser [Mirror]
parseMirrors = some parseMirror 

-- Reads the test input
readInputs :: IO [Mirror]
readInputs = do
    contents <- readFile testInputFile
    return $ runParser parseMirrors contents

-- Returns the column of reflection (or O if there is none)
findVerticalReflection :: Mirror -> Int
findVerticalReflection xss = 0

-- Returns the row of the reflection (or 0 if there is none)
findHorizontalReflection :: Mirror -> Int
findHorizontalReflection xss = findHorizontalReflection' 0 xss where
    findHorizontalReflection' n xss | n == length xss = 0
                                    | top == bot      = n
                                  where
                                      top = take mn $ reverse (take n xss)
                                      bot = take mn $ drop n xss
                                      mn  = min n (length xss - n) 

-- The solver for part #1 of the puzzle
solvePart1 :: [Mirror] -> Int
solvePart1 hs = sum [sum leftColumns, sum topRows * 100] where
    leftColumns = map findVerticalReflection hs
    topRows     = map findHorizontalReflection hs

-- The solver for part #2 of the puzzle
solvePart2 :: [Mirror] -> Int
solvePart2 hs = 0

-- The full solver
day13Solver :: IO [Int]
day13Solver = do
    input <- readInputs
    putStrLn $ show input

    let mirror = input !! 1
    let refl   = findHorizontalReflection mirror
    putStrLn $ show refl
    return [solvePart1 input, solvePart2 input]

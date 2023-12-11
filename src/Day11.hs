module Day11 (day11Solver) where

import Parser

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day11_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day11_input.txt"

-- An image
type Image = [[Char]]

-- Parses an image
parseImage :: Parser Image
parseImage = do
    xss <- some $ parseRow
    return xss
    where
        parseRow = do 
            parseSpaces 
            xs <- some $ parseIsOneOf "#."
            return xs

-- Reads the test input
readInputs :: IO Image
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseImage contents

-- Returns the width of the image
width :: Image -> Int
width xss = length $ xss !! 0

-- Returns the height of the image
height :: Image -> Int
height xss = length xss

-- Returns the row 
row :: Image -> Int -> [Char]
row xss y = xss !! y

-- Returns the column
col :: Image -> Int -> [Char]
col xss x = [xss `at` (x, y) | y <- [0..height xss - 1]]

-- Returns the image cell value at the specified coordinates
at :: Image -> (Int, Int) -> Char
xss `at` (x, y) = (xss !! y) !! x

-- Returns the list of all the galaxies within the image
galaxies :: Image -> [(Int, Int)]
galaxies xss = [(x, y) | y <- [0..height xss - 1], x <- [0..width xss - 1], (xss `at` (x, y)) == '#'] 

-- Returns all empty row indices
emptyRows :: Image -> [Int]
emptyRows xss = [y | y <- [0..height xss - 1], all (=='.') (row xss y)]

-- Returns all empty column indices
emptyCols :: Image -> [Int]
emptyCols xss = [x | x <- [0..height xss - 1], all (=='.') (col xss x)]

-- Expands the universe based on empty rows and columns
expandGalaxies :: [Int] -> [Int] -> Int -> [(Int, Int)] -> [(Int, Int)]
expandGalaxies er ec f gs = map (expandGalaxy er ec f) gs where
    expandGalaxy er ec f (x, y) = (x + f * (length (filter (<x) ec)), y + f * (length (filter (<y) er)))

-- Returns the manhattan distance between two points
distance :: (Int, Int) -> (Int, Int) -> Int
distance (x0, y0) (x1, y1) = (abs (x0 - x1)) + (abs (y0 - y1))

-- Draws the image
drawImage :: Image -> IO ()
drawImage xss = mapM_ drawImageRow [0..length xss - 1] where
    drawImageRow y = putStrLn $ xss !! y

-- Draws the universe
drawUniverse :: [(Int, Int)] -> IO ()
drawUniverse gs = mapM_ drawRow [ym..yM] where
    drawRow y = putStrLn $ map drawCell [(x,y) | x <- [xm..xM]]
    drawCell c = case elem c gs of
                  True  -> '#'
                  False -> '.'
    (xm, xM)   = (minimum $ map fst gs, maximum $ map fst gs)
    (ym, yM)   = (minimum $ map snd gs, maximum $ map snd gs)

-- Generates all possible pairs for n elements
pairs :: Int -> [(Int, Int)]
pairs n = [(i, j) | i <- [0..n - 1], j <- [0..n - 1], i /= j]

-- The solver for part #1 of the puzzle
solvePart1 :: Image -> Int
solvePart1 xss = (sum $ ds) `div` 2 where
    ds  = map (\(i, j) -> distance (gs' !! i) (gs' !! j)) ps
    ps  = pairs (length gs')
    gs' = expandGalaxies er ec 1 gs
    er  = emptyRows xss
    ec  = emptyCols xss
    gs  = galaxies xss 

-- The solver for part #2 of the puzzle
solvePart2 :: Image -> Int
solvePart2 xss = (sum $ ds) `div` 2 where
    ds  = map (\(i, j) -> distance (gs' !! i) (gs' !! j)) ps
    ps  = pairs (length gs')
    gs' = expandGalaxies er ec (1000000 - 1) gs
    er  = emptyRows xss
    ec  = emptyCols xss
    gs  = galaxies xss 

-- The full solver
day11Solver :: IO [Int]
day11Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

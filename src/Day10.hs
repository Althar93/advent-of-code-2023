module Day10 (day10Solver) where

import Parser
import Data.List (find)
import Data.Maybe (isJust)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day10_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day10_input.txt"

-- A single tile
type Tile = Char

-- A map
type Map = [[Tile]]

-- Parses a map
parseMap :: Parser Map
parseMap = do
    xss<- some $ parseMapRow
    return xss
    where
        parseMapRow = do 
            parseSpaces 
            xs <- some $ parseIsOneOf "|-LJ7F.S"
            return xs

-- Reads the test input
readInputs :: IO Map
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseMap contents

-- Returns the width of the map
width :: Map -> Int
width xss = length (xss !! 0)

-- Returns the height of the map
height :: Map -> Int
height xss = length xss

-- Returns the tile at the specified location
at :: Map -> (Int, Int) -> Char
xss `at` (x, y) = (xss !! y) !! x

-- North direction
north :: (Int, Int)
north = (0, -1)

-- South direction
south :: (Int, Int)
south = (0, 1)

-- East direction
east :: (Int, Int)
east = (1, 0)

-- West direction
west :: (Int, Int)
west = (-1, 0)

-- Returns all possible paths for a given tile in the map
paths :: Map -> (Int,Int) -> [(Int, Int)]
paths xss (x, y) = case xss `at` (x, y) of
                     'S' -> filter (\ps -> (x, y) `elem` (paths xss ps)) $ map wrap [north, east, south, west]
                     '|' -> map wrap [north, south]
                     '-' -> map wrap [east, west]
                     'L' -> map wrap [north, east]
                     'J' -> map wrap [north, west]
                     '7' -> map wrap [south, west]
                     'F' -> map wrap[south, east]
                     '.' -> []
                     where
                         wrap (xo, yo) = ((x + xo) `mod` (width xss), (y + yo) `mod` (height xss))
                   
-- Returns the unique non-backtracking path
pathFrom :: Map -> (Int, Int) -> (Int, Int) -> (Int, Int)
pathFrom xss p0 p = case filter (not . (==p0)) (paths xss p) of
                      [p1]      -> p1
                      otherwise -> error "Invalid path"

-- Finds the start
findStart :: Map -> (Int, Int)
findStart xss = case [(x, y) | x <- [0..width xss - 1], y <- [0..height xss - 1], (xss `at` (x, y) == 'S')] of
                  [s]       -> s
                  otherwise -> error "Could not find start"

-- Travels the pipe from the specified starting node and direction and return the full loop
travelPipe :: Map -> [(Int, Int)] -> [(Int, Int)]
travelPipe xss (p:ps) | length ps == 0 = travelPipe xss ((head (paths xss p)):p:ps) 
                      | p == last ps   = reverse (p:ps)
                      | otherwise      = travelPipe xss (p':p:ps) where
                          p' = head $ filter (\x -> x /= head ps) (paths xss p) 

-- Computes the pipe distance
distance :: [(Int, Int)] -> Int
distance xss = (length xss - 1) `div` 2

-- Returns all spaces which are NOT part of the loop
outerSpaces :: Map -> [(Int, Int)] -> [(Int, Int)]
outerSpaces xss ls = [(x, y) | x <- [0..width xss - 1], y <- [0..height xss - 1], not ((x, y) `elem` ls)]

-- Returns whether a point is enclosed within the specified loop
isEnclosed :: Map -> [(Int, Int)] -> (Int, Int) -> Bool
isEnclosed xss ls (x, y) = odd $ min northPipes southPipes where
    -- #TODO : Add logic to add 'S' in the correct section - here I hardcode S for the puzzle input
    northPipes = length $ filter (\(xp, yp) -> elem (xss `at` (xp, yp)) "|LJS") row
    southPipes = length $ filter (\(xp, yp) -> elem (xss `at` (xp, yp)) "|7F") row
    row        = [(xp, y) | xp <- [x..width xss - 1], elem (xp, y) ls]

-- The solver for part #1 of the puzzle
solvePart1 :: Map -> Int
solvePart1 xss = distance loop where
    loop = travelPipe xss [findStart xss]

-- The solver for part #2 of the puzzle
solvePart2 :: Map -> Int
solvePart2 xss = length enclosedTiles where
    enclosedTiles = filter (isEnclosed xss loop) yss
    yss           = outerSpaces xss loop
    loop          = travelPipe xss [findStart xss]

-- The full solver
day10Solver :: IO [Int]
day10Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

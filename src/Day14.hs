module Day14 (day14Solver) where

import Parser

import Data.List (elemIndex, transpose)
import Data.Maybe (fromMaybe)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day14_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day14_input.txt"

-- A single board
type Board = [[Char]]

-- Parses a single board
parseBoard :: Parser Board
parseBoard = do
    parseSpaces
    xss <- some parseBoardRow
    return xss
    where
        parseBoardRow = do
          parseSpaces
          xs <- some $ parseIsOneOf ".#O"
          return xs

-- Reads the test input
readInputs :: IO Board
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseBoard contents

-- Computes the total load on the north support beams
totalLoad :: Board -> Int
totalLoad xss = sum $ zipWith (*) [n, n - 1..] (map length oss) where
  n   = length xss
  oss = map (filter (=='O')) xss 

-- Mirrors the board vertically
mirror :: Board -> Board
mirror xss = map reverse xss

-- Tilts the board west
tiltWest :: Board -> Board
tiltWest xss = map tiltRowLeft xss

-- Tilts the board east
tiltEast :: Board -> Board
tiltEast xss = mirror (tiltWest (mirror xss))

-- Tilts the board north
tiltNorth :: Board -> Board
tiltNorth xss = transpose (tiltWest (transpose xss))

-- Tilts the board south
tiltSouth :: Board -> Board
tiltSouth xss = (transpose . mirror) (tiltWest ((mirror . transpose) xss))

-- Runs a single tilt cycle
tiltCycle :: Board -> Board
tiltCycle = tiltEast . tiltSouth . tiltWest . tiltNorth

-- Runs the tilt a certain number of times, find a period and predicts the final state based on cycle history
tiltCycles :: Int -> Board -> Board
tiltCycles n xss = tiltCycles' (n - 1) 0 [] xss where
  tiltCycles' n k yss xss | k == n     = xss
                          | otherwise  = case elemIndex xss' yss of
                            Just i -> yss !! (i + (n - i) `mod` (k - i))
                            Nothing -> tiltCycles' n (k + 1) (yss++[xss']) xss'
                          where
                            xss' = tiltCycle xss

-- Tilts a row to the left
tiltRowLeft :: [Char] -> [Char]
tiltRowLeft xs = tiltRowLeft' 0 xs where
  tiltRowLeft' n []     = replicate n '.'
  tiltRowLeft' n (x:xs) = case x of
    '.' -> tiltRowLeft' (n + 1) xs
    '#' -> (replicate n '.') ++ '#' :  tiltRowLeft' 0 xs
    'O' -> 'O' : tiltRowLeft' 0 ((replicate n '.') ++ xs)

tiltAndDrawBoard :: Board -> Int -> IO ()
tiltAndDrawBoard xss n = do 
  putStrLn $ "\n\nCycles : " ++ (show n) 
  let xss' = tiltCycles n xss
  mapM_ putStrLn xss'

-- The solver for part #1 of the puzzle
solvePart1 :: Board -> Int
solvePart1 = totalLoad . tiltNorth

-- The solver for part #2 of the puzzle
solvePart2 :: Board -> Int
solvePart2 = totalLoad . (tiltCycles 1000000000)

-- The full solver
day14Solver :: IO [Int]
day14Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
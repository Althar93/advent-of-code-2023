module Day6 (day6Solver) where

import Parser

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day6_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day6_input.txt"

-- A single race
type Race = (Int, Int)

-- Parses multiple races
parseRaces :: Parser [Race]
parseRaces = do
  parseString "Time:"
  ts <- some $ parseNumber
  parseSpaces
  parseString "Distance:"
  ds <- some $ parseNumber
  return $ zip ts ds
  where
    parseNumber = do
      parseSpaces
      n <- parseInt
      return n

-- Reads the test input
readInputs :: IO [Race]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseRaces contents

-- Combines the races into one for part 2
combineRaces :: [Race] -> Race
combineRaces xs = (ts', ds') where
  ts'        = read $ concatMap show ts
  ds'        = read $ concatMap show ds
  (ts, ds)   = unzip xs

-- Returns the number of ways to win a race using brute force
winWays :: Race -> Int
winWays (t, dM) = hM - hm - 1 where
    (hm, _) = last $ takeWhile (\(_, d) -> d <= dM) [race t h | h <- [1..]]
    (hM, _) = last $ takeWhile (\(_, d) -> d <= dM) [race t h | h <- [t-1, t-2..0]]
    race t h = (h, (t - h) * h)

-- Returns the number of ways to win by just solving the quadratic describing the race
-- The problem is of the form : '-h^2 + t * h - dM' where a = -1, b = t & c = -dM
winWaysQuadratic :: Race -> Int
winWaysQuadratic (t, dM) = (ceiling hM) - (floor hm) - 1 where
  hm        = (-b + sqrt(delta)) / (2.0 * a)
  hM        = (-b - sqrt(delta)) / (2.0 * a)
  delta     = b * b - 4.0 * a * c
  a         = fromIntegral $ -1
  b         = fromIntegral $ t
  c         = fromIntegral $ -dM
  
-- The solver for part #1 of the puzzle
solvePart1 :: [Race] -> Int
solvePart1 xss = product $ map winWaysQuadratic xss 

-- The solver for part #2 of the puzzle
solvePart2 :: [Race] -> Int
solvePart2 xss = winWaysQuadratic $ combineRaces xss

-- The full solver
day6Solver :: IO [Int]
day6Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
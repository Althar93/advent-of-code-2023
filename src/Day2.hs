module Day2 (day2Solver) where

import Parser
import Data.Maybe

import Data.Char (isAlphaNum)
import Data.List (sum)
import Data.Text (pack, unpack, replace)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day2_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day2_input.txt"

-- A single game consisting of an id and a list of subsets
data Game = Game Int [(Int, Int, Int)] deriving (Show)

-- Parses a single game
parseGame :: Parser Game
parseGame = do
    parseSpaces
    parseString "Game "
    id <- parseInt
    xs <- some parseSingleGame
    return $ Game id xs
    where
        parseSingleGame = do
            parseIsOneOf ":;"
            ns <- some $ parseMore <|> parseOne
            return $ foldr (\(r, g, b) (r1, g1, b1) -> (r + r1, g + g1, b + b1)) (0, 0, 0) ns
        parseMore = do
            p <- parseOne
            parseString ", "
            return p 
        parseOne = do
            parseSpaces
            n  <- parseInt
            parseSpaces
            mt <- some $ parseIs isAlphaNum
            case mt of
              "red"   -> return (n, 0, 0)
              "green" -> return (0, n, 0)
              "blue"  -> return (0, 0, n) 

-- Parses some games
parseGames :: Parser [Game]
parseGames = some $ parseGame
    
-- Reads the test input
readInputs :: IO [Game]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseGames contents

-- The solver for part #1 of the puzzle
solvePart1 :: [Game] -> Int
solvePart1 xs = sum ids where
    ids                                = map (\(Game id _) -> id) possibleGames
    possibleGames                      = filter (isPossible (12, 13, 14)) xs
    isPossible p (Game _ ps)           = all (isPossible' p) ps
    isPossible' (rM, gM, bM) (r, g, b) = r <= rM && g <= gM && b <= bM

-- The solver for part #2 of the puzzle
solvePart2 :: [Game] -> Int
solvePart2 xs = sum powers where
    powers                             = map (\(r, g, b) -> r * g * b) minPossibleGames
    minPossibleGames                   = map minGame xs
    minGame (Game _ ps)                = foldr (\(r1, g1, b1) (r, g, b) -> (max r1 r, max g1 g, max b1 b)) (head ps) ps  

-- The full solver
day2Solver :: IO [Int]
day2Solver = do
    input <- readInputs
    putStrLn $ show input
    return [solvePart1 input, solvePart2 input]

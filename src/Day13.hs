module Day13 (day13Solver) where

import Parser

import Data.List (transpose)
import Data.Maybe (fromMaybe)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day13_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day13_input.txt"

-- A single mirror
type Mirror = [[Bool]]

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
            return $ map toBoolean xs

-- Converts a symbol to its boolean counterpart
toBoolean :: Char -> Bool
toBoolean '.' = False
toBoolean '#' = True

-- Converts a boolean back to its symbol
toSymbol :: Bool -> Char
toSymbol False = '.'
toSymbol True  = '#'

showMirror :: Mirror -> [[Char]]
showMirror xss = map (map toSymbol) xss

-- Parses some mirrors
parseMirrors :: Parser [Mirror]
parseMirrors = some parseMirror 

-- Reads the test input
readInputs :: IO [Mirror]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseMirrors contents

-- Returns the row of reflection (or Nothing if there is none)
findHorizontalReflection :: Int -> Mirror -> Int
findHorizontalReflection k xss = findHorizontalReflection' k 1 xss where
  findHorizontalReflection' k n xss | (n == length xss) = 0
                                  | (k == n) = findHorizontalReflection' k (n+1) xss
                                  | otherwise         = case countDifferences' top bot of
                                    0 -> n
                                    _ -> findHorizontalReflection' k (n+1) xss
                                  where
                                    top = reverse $ take n xss
                                    bot = drop n xss

-- Finds reflections in the mirror and returns the corresponding score
findReflection :: (Int, Int) -> Mirror -> (Int, Int)
findReflection (tr, lc) xss = (findHorizontalReflection tr xss, findHorizontalReflection lc (transpose xss))

-- Computes the score of the reflection
scoreReflection :: (Int, Int) -> Int
scoreReflection (0, lc) = lc
scoreReflection (tr, _) = tr * 100
--scoreReflection  _      = error "Cannot score reflection"

fixHorizontalSmudge :: Int -> Mirror -> Maybe Mirror
fixHorizontalSmudge k xss = fixHorizontalSmudge' k 1 xss where
  fixHorizontalSmudge' k n xss  | (n == length xss) = Nothing
                                | (n == k)          = fixHorizontalSmudge' k (n+1) xss     
                                | otherwise         = case countDifferences' top bot of
                                  1 -> case countDifferences' top' bot' of
                                    0 -> Just $ (reverse top') ++ bot
                                    _ -> error $ "Mismatch still between :\n" ++ (show (showMirror top')) ++ "\nand xssb:\n" ++ (show (showMirror bot'))
                                  _ -> fixHorizontalSmudge' k (n+1) xss     
                                where
                                  top   = reverse $ take n xss
                                  bot   = drop n xss
                                  delta = differences' top bot
                                  top'  = applyDifferences' top delta
                                  bot'  = bot
                                
-- Fixes the smudge in the mirror
fixSmudge :: (Int, Int) -> Mirror -> Mirror
fixSmudge (tr, lc) xss = case fixHorizontalSmudge tr xss of
  Just xss' -> xss'
  Nothing   -> case fixHorizontalSmudge lc (transpose xss) of
    Just yss' -> (transpose yss')
    Nothing   -> error "No smudge"

-- Count the differences between two lists of lists
countDifferences' :: [[Bool]] -> [[Bool]] -> Int
countDifferences' xss yss = sum $ zipWith countDifferences xss yss

-- Counts the differences between two lists
countDifferences :: [Bool] -> [Bool] -> Int
countDifferences xs ys = length $ filter (==True) (differences xs ys) 

differences' :: [[Bool]] -> [[Bool]] -> [[Bool]]
differences' xss yss = zipWith differences xss yss

-- Computes the difference mask
differences :: [Bool] -> [Bool] -> [Bool]
differences xs ys = zipWith xor xs ys where
  xor a b = if a /= b then True else False

applyDifferences' :: [[Bool]]-> [[Bool]] -> [[Bool]]
applyDifferences' [] []             = []
applyDifferences' (xs:xss) []       = (xs:xss)
applyDifferences' (xs:xss) (ds:dss) = (applyDifferences xs ds) : (applyDifferences' xss dss)
applyDifferences' []        dss     = error $ "Not all differences applied " ++ (show dss)

-- Applies differences to a boolean mask
applyDifferences :: [Bool] -> [Bool] -> [Bool]
applyDifferences xs ds = zipWith toggle xs ds where
  toggle a b = if b then not a else a
      
-- The solver for part #1 of the puzzle
solvePart1 :: [Mirror] -> Int
solvePart1 hs = sum $ map scoreReflection rs where
  rs = map (findReflection (0,0)) hs

-- The solver for part #2 of the puzzle
solvePart2 :: [Mirror] -> Int
solvePart2 hs = sum $ map scoreReflection rs' where
  rs' = zipWith findReflection rs hs'
  hs' = zipWith fixSmudge rs hs
  rs  = map (findReflection (0,0)) hs

-- The full solver
day13Solver :: IO [Int]
day13Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
module Day15 (day15Solver) where

import Parser

import Data.Char (ord, isAlphaNum)
import Data.List (findIndex, deleteBy, zipWith)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day15_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day15_input.txt"

parseInput :: Parser [Int]
parseInput = error "Not implemented"

-- A single hash
type Hash = String

-- A lens consisting of an identifier and focal length
type Lens = (String, Int)

-- A single box containing multiple lens 'slots'
type Box = [Lens]

-- A single instruction
data Instruction = ISet String Int | IRemove String deriving (Show) 

-- Parses a single instruction
parseInstruction :: Parser Instruction
parseInstruction = do
    l <- some $ parseIs isAlphaNum
    i <- parseIsOneOf "=-"
    case i of
      '-' -> return $ IRemove l
      '=' -> do
          fl <- parseInt
          return $ ISet l fl

-- Parses a single hash
parseHash :: Parser Hash
parseHash = some $ parseIs (not . (==','))

-- Parses some hashes
parseHashes :: Parser [Hash]
parseHashes = some $ parseMore <|> parseHash where
    parseMore = do
        parseChar ','
        parseHash

-- Reads the test input
readInputs :: IO [Hash]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseHashes contents

-- Computes the corresponding hash value
hash :: Hash -> Int
hash = foldl hashValue 0 where
    hashValue v c = v'' `mod` 256 where
        v'' = v' * 17
        v'  = v + ord c

-- Computes the focusing power of lense
focusingPower :: Int -> Int -> Int -> Int
focusingPower bn sn fl = (1 + bn) * (1 + sn) * fl

-- Computes the total focusing power of lenses
totalFocusingPower :: [Box] -> Int
totalFocusingPower xs = sum $ zipWith boxFocusingPower [0..] xs where
    boxFocusingPower bn ys = sum $ zipWith (slotFocusingPower bn) [0..] ys where
        slotFocusingPower bn sn (_, fl) = focusingPower bn sn fl

-- Removes the lens with the specified label
removeLens :: String -> Box -> Maybe Box
removeLens l xs = case findIndex ((==l) . fst) xs of
                    Nothing -> Nothing
                    Just i  -> Just $ (take i xs) ++ (drop (i + 1) xs)

-- Sets the value of an existing' lens in a box
setLens :: String -> Int -> Box -> Maybe Box
setLens l fl xs = case findIndex ((==l) . fst) xs of
                    Nothing -> Just $ xs ++ [ls]
                    Just i  -> Just $ (take i xs) ++ [ls] ++ (drop (i + 1) xs)
                    where
                        ls = (l, fl)

-- Adds a lens to the box
addLens :: String -> Int -> Box -> Maybe Box
addLens l fl xs = Just $ xs ++ [ls] where
    ls = (l, fl)

-- Maps the function over the list until it completes
mapFirstMaybe :: (a -> Maybe a) -> [a] -> Maybe [a]
mapFirstMaybe f []     = Nothing
mapFirstMaybe f (x:xs) = case f x of
                          Nothing -> case mapFirstMaybe f xs of
                                       Nothing  -> Nothing
                                       Just xs' -> Just $ (x:xs')
                          Just x' -> Just $ (x':xs)

-- Executes the function on the element at the specified index
atFirstMaybe :: (a -> Maybe a) -> Int -> [a] -> Maybe [a]
atFirstMaybe f i []     = Nothing 
atFirstMaybe f i (xs)   = case f (xs !! i) of
                            Nothing -> Nothing
                            Just x' -> Just $ (take i xs) ++ [x'] ++ (drop (i + 1) xs)

-- Steps the boxes using a single instruction
step :: [Box] -> Instruction -> [Box]
step xs (IRemove l) = case mapFirstMaybe (removeLens l) xs of
                        Just xs' -> xs'
                        Nothing  -> xs
step xs (ISet l fl) = case atFirstMaybe (setLens l fl) (hash l) xs of
                        Just xs' -> xs'
                        Nothing  -> case atFirstMaybe (addLens l fl) (hash l) xs of
                                     Just xs' -> xs'
                                     Nothing  -> error "Shouldn't be here" 

-- The solver for part #1 of the puzzle
solvePart1 :: [Hash] -> Int
solvePart1 hs = sum $ map hash hs

-- The solver for part #2 of the puzzle
solvePart2 :: [Hash] -> Int
solvePart2 hs = totalFocusingPower bs' where
    bs' = foldl step bs is
    bs  = replicate 256 []
    is  = map (runParser parseInstruction) hs

-- The full solver
day15Solver :: IO [Int]
day15Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

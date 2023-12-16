module Day16 (day16Solver) where

import Parser

import Data.List (find, (\\), nub)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day16_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day16_input.txt"

-- A single board
type Board = [[Char]]

-- A direction
data Direction = North | East | South | West deriving (Show, Eq)

-- A board tile type
data BoardTileType = Empty | MirrorNESW | MirrorNWSE | SplitterNS | SplitterEW deriving (Show, Eq)

-- A board tile
type BoardTile     = ((Int, Int), BoardTileType)

-- An energised tile
type EnergisedTile = ((Int, Int), Direction)

-- A light beam denoted as a path of energised tiles
type LightBeam = [EnergisedTile]

-- Parses a single board
parseBoard :: Parser Board
parseBoard = do
    parseSpaces
    xss <- some parseBoardRow
    return xss
    where
        parseBoardRow = do
          parseSpaces
          xs <- some $ parseIsOneOf ".|-\\/"
          return xs

-- Reads the test input
readInputs :: IO Board
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseBoard contents

-- Samples the 2D list at the specified indices
at :: [[a]] -> (Int, Int) -> a
xss `at` (x, y) = (xss !! y) !! x

-- Returns the width of the 2D list
width :: [[a]] -> Int
width xss = length $ xss !! 0

-- Returns the height of the 2D list
height :: [[a]] -> Int
height xss = length xss

-- Offsets the given position in the given direction
offset :: Direction -> (Int, Int) -> (Int, Int)
offset North (x, y) = (x, y - 1)
offset East  (x, y) = (x + 1, y)
offset South (x, y) = (x, y + 1)
offset West  (x, y) = (x - 1, y)

-- Generate a trail
collision :: Board -> EnergisedTile -> Maybe BoardTileType
collision xs ((x, y), _) | x < 0 || x >= width xs || y < 0 || y >= height xs = Nothing
                         | otherwise = case xs `at` (x, y) of
                            '.'  -> Just $ Empty
                            '/'  -> Just $ MirrorNESW
                            '\\' -> Just $ MirrorNWSE
                            '-'  -> Just $ SplitterNS
                            '|'  -> Just $ SplitterEW

-- Traces the beam and returns the full path in a BFS fashion
beam :: Board -> ([EnergisedTile], [EnergisedTile]) -> [EnergisedTile]
beam xs ([], zs) = zs
beam xs (ys, zs) = beam xs (ys', zs') where
  zs' = ys ++ zs
  ys' = ((nub . concat) $ map (stepBeam xs) ys) \\ zs

-- Steps a beam once 
stepBeam :: Board -> EnergisedTile -> [EnergisedTile]
stepBeam xs t@(p, d) = case (collision xs) (p', d) of
  Nothing          -> []
  Just Empty       -> [(p', d)]
  Just MirrorNESW  -> case d of
    North -> [(p', East)]
    East  -> [(p', North)]
    South -> [(p', West)]
    West  -> [(p', South)]
  Just MirrorNWSE  -> case d of
    North -> [(p', West)]
    East  -> [(p', South)]
    South -> [(p', East)]
    West  -> [(p', North)]
  Just SplitterNS  -> case d of
    East  -> [(p', d)]
    West  -> [(p', d)]
    _     -> [(p', East), (p', West)]
  Just SplitterEW  -> case d of
    North  -> [(p', d)]
    South  -> [(p', d)]
    _      -> [(p', North), (p', South)]
  where 
    p' = offset d p

-- Returns the number of energised tiles
energisedTileCount :: [EnergisedTile] -> Int
energisedTileCount xs = (length . nub) (map fst xs)

-- Draws the board using the specified char mapping function
drawBoard :: ((Int, Int) -> Char) -> Board -> IO ()
drawBoard f xs = mapM_ (drawRows xs) [0..height xs - 1] where
    drawRows xs y    = putStrLn $ map (drawCell xs y) [0..width xs - 1]
    drawCell xs y x  = f (x, y)
  
-- The solver for part #1 of the puzzle
solvePart1 :: Board -> Int
solvePart1 xs = energisedTileCount ys - 1 where
  ys = beam xs ([((-1, 0), East)], [])

-- The solver for part #2 of the puzzle
solvePart2 :: Board -> Int
solvePart2 xs = maximum (map energisedTileCount es) - 1 where
  es        = map (\s -> beam xs ([s], [])) ss
  ss        = leftEdge ++ topEdge ++ rightEdge ++ botEdge
  leftEdge  = [((-1,y), East)         | y <- [0..height xs - 1]]
  rightEdge = [((width xs,y), West)   | y <- [0..height xs - 1]]
  topEdge   = [((x,-1), South)        | x <- [0..width xs - 1 ]]
  botEdge   = [((x,height xs), North) | x <- [0..width xs - 1 ]]
  
-- The full solver
day16Solver :: IO [Int]
day16Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
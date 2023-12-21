module Day17 (day17Solver) where

import Parser
import Data.Char (digitToInt, intToDigit)
import Data.Maybe (catMaybes, fromJust)
import Data.List (sortBy, deleteBy, delete, find)
import Data.Function (on)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day17_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day17_input.txt"

-- A map
type Map = [[Int]]

-- A direction
data Direction = North | East | South | West deriving (Show, Eq, Enum, Bounded)

-- A node in the pathing
data Node = Node {
    pos     :: !(Int, Int),
    dir     :: !Direction,
    inertia :: !Int,
    prev    :: !(Maybe Node),
    costG   :: !Int,
    costH   :: !Int
} deriving (Eq, Show)

-- Parses a map
parseMap :: Parser Map
parseMap = do
    xss <- some parseMapRow
    return xss
    where
        parseMapRow :: Parser [Int]
        parseMapRow = do
            parseSpaces
            xs <- some parseDigit
            return $ map digitToInt xs

-- Reads the test input
readInputs :: IO Map
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseMap contents

-- Returns the item at the location in the list
at :: [[a]] -> (Int, Int) -> a
xss `at` (x, y) = (xss !! y) !! x
  
-- Returns whether the position is inside the boundaries of the map
isInside :: Map -> (Int, Int) -> Bool
isInside xss (x, y) = x >= 0 && x <= (length . head) xss - 1 && y >= 0 && y <= length xss - 1

-- Creates a new node
mkNode :: (Int, Int) -> Direction -> Int -> Int -> Int -> Maybe Node -> Node
mkNode p d i g h mp = Node { pos = p, dir = d, costG = g, costH = h, prev = mp, inertia = i }

-- Unfolds a chain of nodes into its list
unfoldNode :: Node -> [Node]
unfoldNode n = case prev n of
                Nothing -> [n] 
                Just n' -> n : unfoldNode n'

-- Returns the cost from the node
costF :: Node -> Int
costF n = costG n + costH n

-- Turns either clockwise (1) or anticlockwise (-1)
turn :: Int -> Direction -> Direction
turn t d = toEnum $ ((fromEnum d) + t) `mod` (fromEnum (maxBound :: Direction) + 1)

-- Returns the offset for each of the directions
offset :: Direction -> (Int, Int) -> (Int, Int)
offset North (x, y) = (x, y - 1)
offset East  (x, y) = (x + 1, y)
offset West  (x, y) = (x - 1, y)
offset South (x, y) = (x, y + 1)

-- Returns the position in front of the input node (accounting for inertia)
forward :: Map -> Int -> (Int, Int) -> Node -> Maybe Node
forward xss iM e n = if iN < iM && isInside xss p then Just (mkNode p d (1 + iN) g h (Just n)) else Nothing where
    p  = offset d (pos n)
    d  = dir n
    g  = (riskCost xss p) + (costG n)
    h  = heuristic xss e p
    iN = inertia n

-- Returns the position at the left of the input node
left :: Map -> (Int, Int) -> Node -> Maybe Node
left xss e n = if isInside xss p then Just (mkNode p d 1 g h (Just n)) else Nothing where
    p = offset d  (pos n)
    d = turn (-1) (dir n)
    g = (riskCost xss p) + (costG n)
    h = heuristic xss e p

-- Returns the position at the right of the input node
right :: Map -> (Int, Int) -> Node -> Maybe Node
right xss e n = if isInside xss p then Just (mkNode p d 1 g h (Just n)) else Nothing where
    p = offset d (pos n)
    d = turn (1) (dir n)
    g = (riskCost xss p) + (costG n)
    h = heuristic xss e p

-- Returns the list of movement options
options :: Map -> (Int, Int) -> (Int, Int) -> Node -> [Node]
options xss (im, iM) e n = if (inertia n) < im then catMaybes [mf] else catMaybes [ml, mf, mr] where
    ml = left xss e n
    mf = forward xss iM e n
    mr = right xss e n 

-- Heuristic function
heuristic :: Map -> (Int, Int) -> (Int, Int) -> Int
heuristic xss (xe, ye) (x, y) = abs(x - xe) + abs(y -ye)

-- Returns the risk fo the given position
riskCost :: Map -> (Int, Int) -> Int
riskCost xss (x, y) = ((xss !! y) !! x)

-- Returns the direction from point a to b
direction :: (Int, Int) -> (Int, Int) -> Direction
direction (x0, y0) (x1, y1) | y1 < y0 = North
                            | x1 > x0 = East
                            | y1 > y0 = South
                            | x1 < x0 = West

-- A* algorithm using the rules for the puzzle about inertia and turning
astar :: Map -> (Int, Int) -> Int -> (Int, Int) -> [Node] -> [Node] -> Maybe Node
astar _   _  _  _ [] cs = Nothing
astar xss is mc e os cs = if (pos n) == e && (inertia n) >= (fst is) then Just n else astar xss is mc e os' cs' where
    cs' = n:cs
    os' = foldl queue (delete n os) (options xss is e n)
    queue a ns = case find (\n' -> (pos n') == (pos ns) && (dir n') == (dir ns) && (inertia n') == (inertia ns)) (os ++ cs) of
                   Nothing -> if costG ns < mc then ns : a else a
                   Just _  -> a
    n = head $ sortBy (compare `on` costF) os

-- Returns the most efficient path
path :: Map -> (Int, Int) -> Int -> (Int, Int) -> (Int, Int) -> ([(Int, Int)], Int)
path xss is mc s e = (reverse ps, c) where
    c  = costG n
    ps = map pos (unfoldNode n)
    n  = fromJust $ astar xss is mc e ss []
    ss = map (\d -> mkNode s d 1 0 0 Nothing) [East, South]

-- Draws the map using the specified char mapping function
drawMap :: ((Int, Int) -> Char) -> Map -> IO ()
drawMap f xs = mapM_ (drawRows xs) [0..length xs - 1] where
    drawRows xs y    = putStrLn $ map (drawCell xs y) [0..length (head xs) - 1]
    drawCell xs y x  = f (x, y)

-- Renders the given path
pathRenderer :: Map -> [Node] -> (Int, Int) -> Char
pathRenderer xss ns p = case find (\x -> (pos x) == p) ns of
                         Just n  -> case dir n of
                                      North -> '^'
                                      East  -> '>'
                                      South -> 'v'
                                      West  -> '<'
                         Nothing -> intToDigit (xss `at` p)

-- The solver for part #1 of the puzzle
solvePart1 :: Map -> Int
solvePart1 xss = snd (path xss is mc start end) where
    mc    = (fst end + snd end) * 9
    is    = (0, 3)
    start = (0, 0)
    end   = ((length . head) xss - 1, length xss - 1)

-- The solver for part #2 of the puzzle
solvePart2 :: Map -> Int
solvePart2 xss = snd (path xss is mc start end) where
    mc    = (fst end + snd end) * 9
    is    = (4, 10)
    start = (0, 0)
    end   = ((length . head) xss - 1, length xss - 1)

-- The full solver
day17Solver :: IO [Int]
day17Solver = do
    input <- readInputs
    --let mc    = 1000000000--(fst end + snd end) * 9
    --let start = (0, 0)
    --let end   = ((length . head) input - 1, length input - 1)
    --let ss    = map (\d -> mkNode start d 1 0 0 Nothing) [East, South]
    --let is    = (4, 10)
    --let n     = fromJust $ astar input is mc end ss []
    --let ns    = unfoldNode n --path xss is mc start end

    --drawMap (pathRenderer input ns) input

    return [solvePart2 input]--[solvePart1 input, solvePart2 input]

module Day18 (day18Solver) where

import Parser

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day18_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day18_input.txt"

-- A colour
type Color = String

-- A direction
data Direction = U | R | D | L deriving (Show, Eq, Enum, Bounded)

-- Winding
type Winding = Int

-- A wall node
type Node = (Float, Float, Winding, Color)

-- A dig instruction
data DigInstruction = DigInstruction { dir :: Direction, steps :: Int, color :: String } deriving (Show)

-- Parses a single dig instruction
parseDigInstruction :: Parser DigInstruction
parseDigInstruction = do
  parseSpaces
  d <- parseDirection
  parseSpaces
  n <- parseInt
  parseSpaces
  parseChar '('
  cs <- some $ parseIsOneOf "#abcdef0123456789"
  parseChar ')'
  return $ DigInstruction { dir = d, steps = n, color = cs }
  where
    parseDirection = do
      d <- parseIsOneOf "UDLR"
      case d of
        'U' -> return U
        'D' -> return D
        'L' -> return L
        'R' -> return R

-- Parses some dig instructions
parseDigInstructions :: Parser [DigInstruction]
parseDigInstructions = some parseDigInstruction

-- Reads the test input
readInputs :: IO [DigInstruction]
readInputs = do
    contents <- readFile testInputFile
    return $ runParser parseDigInstructions contents

-- Makes a new node
mkNode :: Float -> Float -> Winding -> Color -> Node
mkNode x y w c = (x, y, w, c)

-- Digs many intructions 
digMany :: Float -> Float -> [DigInstruction] -> [Node]
digMany _  _  []     = []
digMany x0 y0 (d:ds) = n : (digMany x1 y1 ds) 
  where
    (x1, y1, _, _)   = n
    n                = digOne x0 y0 d

-- Digs a single instruction
digOne :: Float -> Float -> DigInstruction -> Node
digOne x0 y0 d = case dir d of
  U -> mkNode x0 (y0 - fromIntegral (steps d)) 0 (color d)
  D -> mkNode x0 (y0 + fromIntegral (steps d)) 0 (color d)
  L -> mkNode (x0 - fromIntegral (steps d)) y0 0 (color d)
  R -> mkNode (x0 + fromIntegral (steps d)) y0 0 (color d)

-- Rebases a list of nodes such that the minimum point sits at (1,1)
rebase :: [Node] -> [Node]
rebase xs = rebase' xs where
  -- Offset
  zm = 1 - min xm ym
  xm = minimum (map (\(x, _, _, _) -> x) xs)
  ym = minimum (map (\(_, y, _, _) -> y) xs)
  -- Rebase
  rebase' []                 = []
  rebase' ((x, y, w, c):xs') = (x + zm, y + zm, w, c) : rebase' xs'

-- Computes the winding of the polygon
wind :: [Node] -> [Node]
wind xs = wind' ((last xs) : xs ++ [head xs]) where
  wind' ((x0, y0, _, _):(x1, y1, w1, c1):(x2, y2, w2, c2):xs') = (x1, y1, w1', c1) :  wind' ((x1, y1, w1, c1):(x2, y2, w2, c2):xs') where
    v01 = (x1 - x0, y1 - y0)
    v21 = (x1 - x2, y1 - y2)
    w1' = (round . signum) $ (fst v21) * (snd v01) - (snd v21) * (fst v01)
  wind' _                                                      = []  
  
-- Computes the area of the polygon using the shoelace algorithm 
computePolygonArea :: [Node] -> Float
computePolygonArea xs = abs(leftSum xs' - rightSum xs') / 2.0 where
  -- Left and right sums
  leftSum  ((x1, y1, _, _):(x2, y2, w2, c2):xs') = x1 * y2 + (leftSum ((x2, y2, w2, c2):xs'))-- (x1 * y2) + (leftSum ((x2, y2, w2, c2):xs'))
  leftSum [(xn, yn,  _, _)]                      = xn * y0
  rightSum ((x1, y1, _, _):(x2, y2, w2, c2):xs') = x2 * y1 + (rightSum ((x2, y2, w2, c2):xs'))--(x2 * y1) + (rightSum ((x2, y2, w2, c2):xs'))
  rightSum [(xn, yn, _, _)]                      = x0 * yn
  -- Initial conditions
  (x0, y0, _, _) = head xs'
  xs'            = xs

-- Computes the volume of the polygon edge
computeEdgeArea :: [Node] -> Float
computeEdgeArea xs = computeEdgeArea' xs where
  computeEdgeArea' ((x1, y1, w1, _):(x2, y2, w2, c2):xs') = (if w1 > 0 then 0.75 else 0.25) + 0.5 * (abs(x2 - x1) + abs(y2 - y1) - 1) + (computeEdgeArea' ((x2, y2, w2, c2):xs'))
  computeEdgeArea' [(xn, yn, wn, _)]                      = (if wn > 0 then 0.75 else 0.25) + 0.5 * (abs(x0 - xn) + abs(y0 - yn) - 1)
  -- Initial conditions
  (x0, y0, _, _) = head xs

-- Computes the full area of the tunnel
computeFullArea :: [Node] -> Float
computeFullArea xs = computePolygonArea xs + computeEdgeArea xs

-- The solver for part #1 of the puzzle
solvePart1 :: [DigInstruction] -> Float
solvePart1 ds = computeFullArea ns' where
  ns' = (wind . rebase) ns
  ns  = digMany 0 0 ds
  -- Winding of the input closing node (1:cw, -1:ccw))
  w = 1

-- The solver for part #2 of the puzzle
solvePart2 :: [DigInstruction] -> Float
solvePart2 xss = 0.0

-- The full solver
day18Solver :: IO [Float]
day18Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

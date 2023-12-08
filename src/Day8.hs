module Day8 (day8Solver) where

import Parser
import Data.Char (isAlphaNum)
import Data.List (sortBy)
import Data.Ord (comparing)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day8_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day8_input.txt"

-- A single instruction
data Instruction = ILeft | IRight deriving (Show)

-- A node
data Node = Node String (String, String) deriving (Eq)

-- Make node an instance of show
instance Show Node where
    show (Node a (b, c)) = a ++ " = (" ++ b ++ ", " ++ c ++ ")"

-- A network of nodes and some instructions
type Network = ([Instruction], [Node])

-- Parses an instruction
parseInstruction :: Parser Instruction
parseInstruction = do
    c <- parseIsOneOf "LR"
    case c of
      'L' -> return ILeft
      'R' -> return IRight

-- Parses a node
parseNode :: Parser Node
parseNode = do
   parseSpaces
   a <- some $ parseIs isAlphaNum
   parseString " = ("
   b <- some $ parseIs isAlphaNum
   parseString ", "
   c <- some $ parseIs isAlphaNum
   parseString ")"
   return $ Node a (b, c) 

-- Parses a network
parseNetwork :: Parser Network
parseNetwork = do
    is <- some $ parseInstruction
    ns <- some $ parseNode
    return (is, ns)

-- Reads the test input
readInputs :: IO Network
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseNetwork contents

-- Returns the location of a node
location :: Node -> String
location (Node l (_, _)) = l

-- Finds and returns the given node
findNode :: Network -> String -> Node
findNode (_, [])     s                     = error $ "Cannot find node " ++ s
findNode (i, (n:ns)) s | s == (location n) = n
                       | otherwise         = findNode (i, ns) s  

-- Steps through a node
stepNode :: Network -> Instruction -> Node -> Node
stepNode ns ILeft  (Node _ (l, _)) = findNode ns l
stepNode ns IRight (Node _ (_, r)) = findNode ns r

-- Naive solver 
naiveSolver :: Network -> Node -> [Node] -> Int -> Int
naiveSolver n@(is, ns) s es k | s `elem` es = k
                              | otherwise   = naiveSolver n s' es (k + 1) where
                                  s' = stepNode n i s
                                  i  = (is !! (k `mod` (length is)))

-- Returns all possible ghost nodes with the matching last symbol
ghostNodes :: Network -> Char -> [Node]
ghostNodes (_, ns) c = filter (\n -> (last . location) n == c) ns 

-- The solver for part #1 of the puzzle
solvePart1 :: Network -> Int
solvePart1 n = naiveSolver n start [end] 0 where
    start = findNode n "AAA"
    end   = findNode n "ZZZ"

-- The solver for part #2 of the puzzle
solvePart2 :: Network -> Int
solvePart2 n = mgcd * supermodulo where
    supermodulo   = product ksdivgdc
    ksdivgdc      = map (\k -> k `div` mgcd) ks
    mgcd          = foldr1 gcd ks
    ks            = map (\x -> naiveSolver n x ends 0) starts
    starts        = ghostNodes n 'A'
    ends          = ghostNodes n 'Z'

-- The full solver
day8Solver :: IO [Int]
day8Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

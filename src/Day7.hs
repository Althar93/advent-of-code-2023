module Day7 (day7Solver) where

import Parser
import Data.List (sort, sortBy, group)
import Data.Ord (comparing)

-- The test input file path for part one
testInputFile :: FilePath
testInputFile = "res/day7_test_input.txt"

-- The input file path
inputFile :: FilePath
inputFile = "res/day7_input.txt"

-- A card type
data Card = CardJoker | Card2 | Card3 | Card4 | Card5 | Card6 | Card7 | Card8 | Card9 | CardT | CardJ | CardQ | CardK | CardA deriving (Eq, Ord, Show)

-- A hand type
data HandType = HighCard Hand | OnePair Hand | TwoPair Hand | ThreeKind Hand | FullHouse Hand | FourKind Hand | FiveKind Hand deriving (Eq, Ord, Show)

-- A hand
type Hand = ([Card], Int)

-- Parses a single card
parseCard :: Parser Card
parseCard = do
    c <- parseIsOneOf "AKQJT98765432"
    case c of
      'A' -> return CardA
      'K' -> return CardK
      'Q' -> return CardQ
      'J' -> return CardJ
      'T' -> return CardT
      '9' -> return Card9
      '8' -> return Card8
      '7' -> return Card7
      '6' -> return Card6
      '5' -> return Card5
      '4' -> return Card4
      '3' -> return Card3
      '2' -> return Card2

-- Parses a single hand
parseHand :: Parser Hand
parseHand = do
    parseSpaces
    xs <- some $ parseCard
    parseSpaces
    n <- parseInt
    return (xs, n)

-- Parses multiple races
parseHands :: Parser [Hand]
parseHands = some $ parseHand

-- Returns the hand type of a given hand
handType' :: Hand -> Hand -> HandType
handType' h0 h   | length sets == 1 = FiveKind h
                 | length sets == 2 = case length (last sets) of
                                        4 -> FourKind h
                                        3 -> FullHouse h
                 | length sets == 3 = case length (last sets) of
                                        3 -> ThreeKind h
                                        2 -> TwoPair h
                 | length sets == 4 = OnePair h                       
                 | otherwise        = HighCard h
    where
        sets = sortHand h0

-- Returns the hand type of a given hand 
handTypeSimple :: Hand -> HandType
handTypeSimple h = handType' h h 

-- Returns the hand type of a given hand accounting for jokers
handTypeJoker :: Hand -> HandType
handTypeJoker (xs, b) = handType' (xs'', b) (xs, b) where
    xs'' = map (\x -> if x == CardJoker then j else x) xs
    j    = case groupSort xs' of
             [] -> CardJoker
             xs -> (head . last) xs
    xs'  = filter (not . (==CardJoker)) xs

-- Sorts a hand according to groupins and value
sortHand :: Hand -> [[Card]]
sortHand (xs, _) = groupSort xs

-- Groups and sorts an array of elements by identical elements in order of length
groupSort :: Ord a => [a] -> [[a]]
groupSort xs = sortBy (comparing length) (group $ sort xs)

-- Converts the 'J' symbol into a joker for the purposes of part 2
convertJoker :: Hand -> Hand
convertJoker (xs, b) = (xs', b) where
    xs' = map (\x -> if x == CardJ then CardJoker else x) xs

-- Reads the test input
readInputs :: IO [Hand]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseHands contents

-- The solver for part #1 of the puzzle
solvePart1 :: [Hand] -> Int
solvePart1 xs = sum $ map (\(r, b) -> r * b) rankedBids' where
    rankedBids' = zip rankedBids [1..]
    rankedBids  = map snd rankedHands
    rankedHands = sortBy (comparing handTypeSimple) xs 

-- The solver for part #2 of the puzzle
solvePart2 :: [Hand] -> Int
solvePart2 xs = sum $ map (\(r, b) -> r * b) rankedBids' where
    rankedBids' = zip rankedBids [1..]
    rankedBids  = map snd rankedHands
    rankedHands = sortBy (comparing handTypeJoker) xs'
    xs'         = map convertJoker xs

-- The full solver
day7Solver :: IO [Int]
day7Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]

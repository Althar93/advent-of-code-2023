module Main where

import Common
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

main :: IO ()
main = do
    putStrLn "== Advent of Code 2023 =="
    --executeAndPrintResults "Day 1"  day1Solver
    --executeAndPrintResults "Day 2"  day2Solver
    --executeAndPrintResults "Day 3"  day3Solver
    --executeAndPrintResults "Day 4"  day4Solver
    --executeAndPrintResults "Day 5"  day5Solver
    --executeAndPrintResults "Day 6"  day6Solver
    --executeAndPrintResults "Day 7"  day7Solver
    --executeAndPrintResults "Day 8"  day8Solver
    executeAndPrintResults "Day 9" day9Solver

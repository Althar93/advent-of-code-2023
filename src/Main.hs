module Main where

import Common
import Day1
import Day2
import Day3

main :: IO ()
main = do
    putStrLn "== Advent of Code 2023 =="
    executeAndPrintResults "Day 1"  day1Solver
    executeAndPrintResults "Day 2"  day2Solver
    executeAndPrintResults "Day 3"  day3Solver


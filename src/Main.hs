module Main where

import Common
import Day1

main :: IO ()
main = do
    putStrLn "== Advent of Code 2023 =="
    executeAndPrintResults "Day 1"  day1Solver

module Main where
import Solver1 (solveInput, parseInput)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ (solveInput . parseInput) input
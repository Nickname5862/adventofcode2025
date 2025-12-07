module Main where
import Solver (solveInput, parseInput)

main :: IO ()
main = do
  input <- readFile "currentDay/input.txt"
  print $ solveInput $ parseInput input
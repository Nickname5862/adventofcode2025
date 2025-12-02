import Solver (solveInput, parseInput)
-- import Utils (solve)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ (solveInput . parseInput) input
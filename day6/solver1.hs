module Solver where
import Utils (takeLength)
import Data.List (transpose)

type Operation = Int -> Int -> Int

type ParsedInput = [([Int], Operation)] -- already transposed

{--
parseInput
1. Take the giant string, split on '\n' (`lines`)
2. Split each line on any number of spaces (' +' in regex)
3. Parse all values in each line but the last to numbers (`takeLength (-1)` and `read`)

Then, solveInput:
4. Transpose the input
5. Apply the functions (`+` and `*` only)
6. Sum the whole
--}

parseInput :: String -> ParsedInput
parseInput input = t' where
    l = lines input
    s = map words l
    t = transpose s
    t' = map (\v -> (map read $ takeLength (-1) v, parseOperation $ last v)) t

solveInput :: ParsedInput -> Int
solveInput = sum . map (uncurry applyOperation)


parseOperation :: String -> Operation
parseOperation "+" = (+)
parseOperation "*" = (*)
parseOperation _   = error "unsupported operation"

applyOperation :: [Int] -> Operation -> Int
applyOperation is op = foldr1 op is
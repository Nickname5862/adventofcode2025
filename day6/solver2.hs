module Solver where
import Utils (takeLength, dropLength)
import Data.List (transpose)
import Data.List.Split (splitOn)

type Operation = Int -> Int -> Int

type ParsedInput = [([Int], Operation)] -- already transposed

{--
parseInput:
1. Take the giant string, split on '\n' (`lines`)
2. Process the last line as the `operations`, and continue with the rest.
3. Transpose the strings to get verticals, and filter the spaces out of them.
4. Then split this on the now-empty verticals, parse the rest to ints, and zip them with the operations.
--}

parseInput :: String -> ParsedInput
parseInput input = zip separateVerticals operations where -- zip the vertical lines with the operations
    ls = lines input -- split on lines
    operations = map parseOperation . words . head . dropLength (-1) $ ls -- separate the last line (operations)
    verticals = map (filter (/= ' ')) . transpose . takeLength (-1) $ ls -- calculate the vertical strings
    separateVerticals = map (map (\v -> read v :: Int)) . splitOn [""] $ verticals -- split on the empty strings, parse to ints

solveInput :: ParsedInput -> Int
solveInput = sum . map (uncurry applyOperation)


parseOperation :: String -> Operation
parseOperation "+" = (+)
parseOperation "*" = (*)
parseOperation _   = error "unsupported operation"

applyOperation :: [Int] -> Operation -> Int
applyOperation is op = foldr1 op is

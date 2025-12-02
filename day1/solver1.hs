module Solver where

type ParsedInput = [(Direction, Int)]

parseInput :: String -> ParsedInput
parseInput = map parseLine . lines where
    parseLine :: String -> (Direction, Int)
    parseLine s = (parseDirection $ take 1 s, read $ drop 1 s)
    parseDirection :: String -> Direction
    parseDirection "R" = DRight
    parseDirection "L" = DLeft
    parseDirection _ = error "Unsupported direction"

solveInput :: ParsedInput -> Int
solveInput input = length $ filter (==0) allDials where
    allDials :: [Dial]
    allDials = scanl (\a (dir, d) -> rotate dir d a) 50 input


data Direction = DLeft | DRight deriving Show
type Distance = Int
type Dial = Int

rotate :: Direction -> Distance -> Dial -> Dial
rotate direction distance initialDial = (initialDial + amount direction distance) `mod` 100 where
    amount :: Direction -> Int -> Int
    amount DRight = id
    amount DLeft = (100 -)
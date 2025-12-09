{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Solver where
import Utils (Coordinate, splitOn)
import Prelude hiding (lookup)

type ParsedInput = [Coordinate]  -- The Map, and the coordinate of the manifold starting position.

{--
Let me guess. Exhaustive is too expensive? Let's see.

Improvements:
1. We currently try every combination twice.
2. There is some cool ordering which you can use to get guarantees, I'm sure

No need, brute-force is quick enough!
--}

parseInput :: String -> ParsedInput
parseInput = map ((\(a:b:_) -> (read a :: Int, read b :: Int)) . splitOn [',']) . lines

solveInput :: ParsedInput -> Int
solveInput input = maximum [area c1 c2 | c1 <- input, c2 <- input]


area :: Coordinate -> Coordinate -> Int
area c1 c2 = abs (fst c1 - fst c2 + 1) * abs (snd c1 - snd c2 + 1)
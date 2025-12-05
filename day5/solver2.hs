{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Solver where
import Utils (splitOn)
import Data.Bifunctor (bimap)
import Data.List (sortOn)

type Range = (Int, Int)
type IngredientID = Int
type ParsedInput = [Range]

parseInput :: String -> ParsedInput
parseInput input = foldl updateRanges [] sortedRanges where
    split = splitOn "\n\n" input
    ranges = map ((\l -> (read $ head l, read $ l !! 1)) . splitOn "-") $ lines $ head split
    sortedRanges = sortOn fst ranges

solveInput :: ParsedInput -> Int
solveInput = foldr (\range -> (snd range - fst range + 1 +)) 0

-- replaces the first range in the existing list it encounters which overlaps with the current range
updateRanges :: [Range] -> Range -> [Range]
updateRanges []     range = [range]
updateRanges (r:ys) range = if fallInRange range r then mergeRanges range r : ys else r : updateRanges ys range

-- This function assumes that the ranges are sorted on fst. Then, we don't need to check that `fst newRange >= fst oldRange`
-- It also eliminates the option that `snd newRange >= fst oldRange && snd newRange <= snd oldRange` instead.
fallInRange :: Range -> Range -> Bool
fallInRange newRange oldRange = fst newRange <= snd oldRange

mergeRanges :: Range -> Range -> Range
mergeRanges r1 = bimap (min (fst r1)) (max (snd r1))


{--
Haha, I knew it! We have to do this anyway ;)

These values are definitely too big to just enumerate. We have to:
- observe whether the start or ends of a new range fall within an existing range;
- then adjust the existing ranges instead of adding new ones;
- and at the end, just subtract the ends of the ranges with the starts, rather than enumerating.

This is not trivial:
[(0, 3), (5, 8)] with an added (3,5) will merge the existing ones too.
Sorting on starting value will fix that issue.
--}
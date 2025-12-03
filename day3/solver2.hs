module Solver2 where
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char (digitToInt)

type ParsedInput = [[Joltage]]

parseInput :: String -> ParsedInput
parseInput input = map (map digitToInt) (lines input)

solveInput :: ParsedInput -> Int
solveInput = sum . map (solveBank 12)


type Joltage = Int
type Jolts = Int
type Index = Int

solveBank :: Int -> [Joltage] -> Jolts
solveBank n joltages = read $ foldr ((\a b -> show a ++ b) . fst) "" (drop 1 $ scanl (\(s, prevI) newN -> findNthJoltage prevI newN joltages) (0, -1) [n, n-1..1])

findNthJoltage :: Int -> Int -> [Joltage] -> (Joltage, Index)
findNthJoltage prevIndex n joltages = (j, i) where
    -- drop the front (already covered) and the end (leave room for the other numbers)
    updatedJoltages = takeLength (\l -> l - n + 1) $ drop (prevIndex + 1) joltages
    j :: Joltage = maximum updatedJoltages
    -- since the index works on the entire `joltages` but we pick from the
    -- `updatedJoltages` which has some dropped some prefix, we need to add that
    -- length, which is `prevIndex + 1`
    i :: Index = prevIndex + 1 + fromJust (elemIndex j updatedJoltages)

--------------------------------------------------

-- FIXME: in Utils
-- Such that you can do e.g. `dropLength (-1)` to drop everything until the last
-- element or the usual `dropLength (const 1)` to simply drop 1 element.
dropLength :: (Int -> Int) -> [a] -> [a]
dropLength f as = drop (f $ length as) as

takeLength :: (Int -> Int) -> [a] -> [a]
takeLength f as = take (f $ length as) as
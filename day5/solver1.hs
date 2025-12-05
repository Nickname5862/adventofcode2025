import Utils (splitOn)

type Range = (Int, Int)
type IngredientID = Int

type ParsedInput = ([Range], [IngredientID])

-- splitOn is probably fucking slow, but it works. Takes a full second since I use it so many times xD
parseInput :: String -> ParsedInput
parseInput input = (ranges,ingredientIds) where
    split = splitOn "\n\n" input
    ranges = map ((\l -> (read $ head l, read $ l !! 1)) . splitOn "-") $ lines $ head split
    ingredientIds = map read $ lines (split !! 1)

-- The simple solution (#1) is quick enough just fine
solveInput :: ParsedInput -> Int
solveInput input = foldr (\iid -> if any (fallInRange iid) (fst input) then (1 +) else (0 +)) 0 $ snd input where
    fallInRange :: IngredientID -> Range -> Bool
    fallInRange iid range = iid >= fst range && iid <= snd range

{--
* Solution 1:
The easy one. Just save the ranges, and for each number, go over all ranges. If it is in any, it is fresh.
Might not be the fastest, but might be okay.

* Solution 2:
When you create the ranges, merge any overlapping ranges together.
It is questionable how many time this saves.
--}
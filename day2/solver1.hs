module Solver where
type ParsedInput = [Range]

-- Split on ',', then split on '-'.
parseInput :: String -> ParsedInput
parseInput input = c where
    a = splitOn "," input
    b = map (splitOn "-") a
    c = map (\v -> (read $ head v, read $ v !! 1)) b

solveInput :: ParsedInput -> Int
solveInput = sum . concatMap solveRange


type Range = (ID, ID)
type ID = Int
type InvalidID = ID

solveRange :: Range -> [Int]
solveRange (start, end) = takeWhile (<= end) (map (read . duplicate . show . (firstInvalidIDHalve start +)) [0..])

-- Calculate the first invalid ID (halve) higher than the starting number of the given range
firstInvalidIDHalve :: Int -> InvalidID
firstInvalidIDHalve start = let n = length (show start) in if even n
    -- if even, take the first half, duplicate it, and check whether it is higher than the original number. If not, add 1 before duplicating.
    -- e.g. 4030 > 4040 and 4050 > 4141
    then let startHalf :: Int = read $ take (n `div` 2) (show start); endHalf :: Int = read $ drop (n `div` 2) (show start) in
        if startHalf >= endHalf then startHalf else startHalf + 1
    -- if uneven, take the number of digits, add 1, and make it start with 1 and end with 0s. Then turn this into an invalid ID.
    -- e.g. 400 > 1010
    else read $ '1' : replicate (n `div` 2) '0'

duplicate :: String -> String
duplicate s = s ++ s

-----------------------------------------------------------------

-- FIXME: put in Utils
-- A function like `splitOn` seems quite useful. `lines` can then be made to be `splitOn "\n"` and words is `splitOn " "`
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn splitter s = splitOn' [] s where
  splitOn' acc [] = [acc]
  splitOn' acc s = let l = length splitter in if take l s == splitter then acc : splitOn' [] (drop l s) else splitOn' (acc ++ take 1 s) (drop 1 s)
  -- FIXME: the `++` is a bit of shame

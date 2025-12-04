import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char (digitToInt)
import Utils (takeLength)

type ParsedInput = [[Joltage]]

parseInput :: String -> ParsedInput
parseInput input = map (map digitToInt) (lines input)

solveInput :: ParsedInput -> Int
solveInput = sum . map solveBank


type Joltage = Int
type Jolts = Int

-- Easy: find the highest number A in the entire line except the last number.
-- Then, find the highest number B in the line starting from the number after A.
solveBank :: [Joltage] -> Jolts
solveBank joltages = jolts where
    firstJoltage :: Joltage = maximum $ takeLength (\l -> l - 1) joltages
    indexOfFirstJoltage :: Int = fromJust $ elemIndex firstJoltage joltages
    secondJoltage :: Joltage = maximum $ drop (indexOfFirstJoltage + 1) joltages
    jolts :: Jolts = read $ show firstJoltage ++ show secondJoltage
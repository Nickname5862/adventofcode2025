import Debug.Trace (trace)

type ParsedInput = [(Direction, Distance)]

parseInput :: String -> ParsedInput
parseInput = map parseLine . lines where
    parseLine :: String -> (Direction, Int)
    parseLine s = (parseDirection $ take 1 s, read $ drop 1 s)
    parseDirection :: String -> Direction
    parseDirection "R" = DRight
    parseDirection "L" = DLeft
    parseDirection _ = error "Unsupported direction"

solveInput :: ParsedInput -> Int
solveInput input = trace (show $ map snd allDials) $ sum $ map snd allDials where
    allDials :: [(Dial, Int)]
    allDials = scanl (\(dial, _) (dir, d) -> rotate dir d dial) (50, 0) input


data Direction = DLeft | DRight deriving Show
type Distance = Int
type Dial = Int

rotate :: Direction -> Distance -> Dial -> (Dial, Int)
rotate DRight distance initialDial = let n = initialDial + distance in (n `mod` 100, n `div` 100)
rotate DLeft distance initialDial = let n = hundredMin initialDial + distance in (hundredMin n `mod` 100, n `div` 100) where
    hundredMin n = (100 - n) `mod` 100



{--
* Solution 1:
Keep adding/removing 1 until you exhaust the number. When you hit 0, add one.
Not quick, but it might be enough.

* Solution 1.5:
Keep adding 100, which definitely adds 1 to our score. When the remaining value is less than 100, increment by step.
Though also for this, you know it is either 0 or 1. If you go Right X amount, and ........??

* Solution 2:
Base it off of the resulting number. Requires more complex calculations though.
Quick, but hard.
No, it's rather easy actually!
Right: going right, the result is simply ((N + P) `div` 100). 99+1 or 0+100 both result in 100 = 1. The reaminder is (N + P) `mod` 100.
Left: going left, the calculation is not the same. 1-1 or 0-100 give different results. Instead, reverse:
      going left, the result is simply ((100 - N + P) `div` 100). The remainder is (100 - ((100 - N + P) `mod` 100)).
      note how we still do +P even though we go left. Since we reverse.
--}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-} -- FIXME: questionable
{-# OPTIONS_GHC -Wno-orphans #-} -- FIXME: questionable
module Utils where
import Data.Map (Map, fromList, toList, insert, keys, lookup, filter)
import Data.List (intercalate, transpose, groupBy, find)
import Prelude hiding (lookup)

-- A function like `splitOn` seems quite useful. `lines` can then be made to be `splitOn "\n"` and words is `splitOn " "`
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn splitter = splitOn' [] where
  splitOn' acc [] = [acc]
  splitOn' acc s = let l = length splitter in if take l s == splitter then acc : splitOn' [] (drop l s) else splitOn' (acc ++ take 1 s) (drop 1 s)
  -- FIXME: the `++` is a bit of shame


-- Such that you can do e.g. `dropLength (-1)` to drop everything until the last
-- element or the usual `dropLength (const 1)` to simply drop 1 element.
dropLength :: Int -> [a] -> [a]
dropLength n as = drop (n + length as) as

takeLength :: Int -> [a] -> [a]
takeLength n as = take (n + length as) as


------------------------------------------------

type CMap a = Map Coordinate a

-- maps a two-dimensional input in the form of "AAA\nBBB" to a Map
-- where a coordinate like (1,0) would map to a char like 'A'
toCoordinateMap :: String -> CMap Char
toCoordinateMap = fromList . concat . zipWith f [0..] . lines where
    f y = zipWith (\x v -> ((x,y), v)) [0..]

toCoordinateMapExcluding :: [Char] -> String -> CMap Char
toCoordinateMapExcluding exclude = Data.Map.filter (not . flip elem exclude) . toCoordinateMap

-- Takes the first character of the string obtained by `show a`
showCoordinateMap :: Show a => CMap a -> String
showCoordinateMap wh = intercalate "\n" groups where
    l :: [(Coordinate, Char)] = map (\(a, b) -> (a, head $ show b)) $ toList wh
    groups :: [String] = transpose $ map (map snd) $ groupBy (\(coord1,_) (coord2,_) -> fst coord1 == fst coord2) l

-- Takes the first character of the string obtained by `show a`
showCoordinateMapWithWalker :: Show a => (Coordinate, a) -> CMap a -> String
showCoordinateMapWithWalker (coord, c) wh = intercalate "\n" groups where
    l :: [(Coordinate, Char)] = map (\(a, b) -> (a, head $ show b)) $ toList (insert coord c wh)
    groups :: [String] = transpose $ map (map snd) $ groupBy (\(coord1,_) (coord2,_) -> fst coord1 == fst coord2) l

-- find an element in a two dimensional grid (assume there is only 1)
-- I assume you already used `toCoordinateMap` to make a Map
coordinateOfChar :: Eq a => CMap a -> a -> Maybe Coordinate
coordinateOfChar m c = find (\key -> c `elem` lookup key m) (keys m)

type Coordinate = (Int, Int)

-- Allow for (+), (-) and (*) to be applied to coordinates
-- e.g. (1,3) * (4,5) = (4,15)
instance Num Coordinate where
  (+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
  (*) (x1,y1) (x2,y2) = (x1 * x2, y1 * y2)
  abs (x1,y1) = (abs x1, abs y1)
  signum (x1, y1) = (signum x1, signum y1)
  fromInteger int = (fromInteger int, fromInteger int)
  negate (x1, x2) = (negate x1, negate x2)

adjacent8 :: [Coordinate]
adjacent8 = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]

adjacent4 :: [Coordinate]
adjacent4 = [(0,1), (1,0), (0,-1), (-1,0)]

data Direction = DDown | DLeft | DUp | DRight

move :: Direction -> Coordinate -> Coordinate
move DDown  = (+ (0,1)) -- might seem counter-intuitive, to have Down do +, but here, the map runs from top to bottom
move DRight = (+ (1,0))
move DUp    = \c -> c - (0,1) -- same for this
move DLeft  = \c -> c - (1,0)

-- replace an element `a` at index `i` in the list `as`. Not trivial in Haskell...
-- help from https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
-- replaceAtIndex :: Int -> a -> [a] -> [a]
-- replaceAtIndex i a as = let (x,_:ys) = splitAt i as in x ++ a : ys


{--

Useful functions:

- map, filter, length
- read, show
- lines, words, length
- all, any/some/idk, not, null
- take, drop
- transpose
- groupBy
- find, loopup
- concat, concatMap
- zipWith
- catMaybes
- <$>, <*>
- maybe
- foldr, scanr, 
- trace, traceShow
- zip, intercalate

--}
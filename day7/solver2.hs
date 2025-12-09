module Solver where
import Utils (Coordinate, CMap, toCoordinateMap, move, Direction (DDown, DLeft, DRight))
import Data.Map (toList, lookup, empty, insert)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Data.Bifunctor (Bifunctor(first, second))

type ParsedInput = (CMap Char, Coordinate) -- The Map, and the coordinate of the manifold starting position.

{--
When you get a coordinate or a splitter, check the coordinates of its neighbor:
- If the coordinate is already in the CMap Int, take this value
- Otherwise, let the coordinate go down until it:
    - Hits a splitter (Just Coordinate), after which you recursively call this function on this splitter. The value returned is also added to the map
    - Hit the end of the screen (Nothing), after which you return 1, and you add this to the map.
--}

parseInput :: String -> ParsedInput
parseInput input = (cmap, startCoordinate) where
    cmap                = toCoordinateMap input
    startCoordinate     = fst . head . filter ((== 'S') . snd) . toList $ cmap

solveInput :: ParsedInput -> Int
solveInput (cmap, startCoordinate) = let c = fromJust . moveTachyonDown cmap $ startCoordinate in snd $ calculateTimelines cmap c empty


-- Can use some improvement, but it works
calculateTimelines :: CMap Char -> Coordinate -> CMap Int -> (CMap Int, Int)
calculateTimelines inputMap coord initialCalculatedMap = foldr f (initialCalculatedMap, 0) [DLeft, DRight] where
    f :: Direction -> (CMap Int, Int) -> (CMap Int, Int)
    f dir (calculatedMap, x) = second (+ x) $ let neighborCoord = move dir coord in case lookup neighborCoord calculatedMap of
        Just v  -> (calculatedMap, v)                                                         -- already in the map, just take the value
        Nothing -> let hitCoord = moveTachyonDown inputMap neighborCoord in case hitCoord of  -- not in the map yet
            Nothing        -> (insert neighborCoord 1 calculatedMap, 1)                       -- hit the border
            Just hitCoord' -> let v' = calculateTimelines inputMap hitCoord' calculatedMap in -- hit a splitter
                first (insert neighborCoord (snd v')) v'

moveTachyonDown :: CMap Char -> Coordinate -> Maybe Coordinate
moveTachyonDown cmap = moveDown where
    moveDown c = case lookup c cmap of
        Nothing  -> Nothing                 -- Out of the screen: Nothing
        Just '^' -> Just c                  -- Splitter: BINGO, return the coordinate
        _        -> moveDown (move DDown c) -- Empty space or S: keep moving down
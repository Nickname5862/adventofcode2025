module Solver where
import Utils (Coordinate, CMap, toCoordinateMap, move, Direction (DDown, DLeft, DRight))
import Data.Map (toList, lookup)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)

type ParsedInput = (CMap Char, Coordinate) -- The Map, and the coordinate of the manifold starting position.

{--
Idea: keep track of the splitters used by keeping them in a list.
If we find a splitter that is already in that list, don't add it again. Ignore it.
When we add a splitter to the list, we also continue recursively on that splitter, applying the same function.
This function adds two tachyon beams and follows them down until they either find the end of the screen or another splitter.
- End of screen    = Stop
- Another splitter = Add it to the list and recurse on it.
--}

parseInput :: String -> ParsedInput
parseInput input = (cmap, startCoordinate) where
    cmap                = toCoordinateMap input
    startCoordinate     = fst . head . filter ((== 'S') . snd) . toList $ cmap

solveInput :: ParsedInput -> Int
solveInput (cmap, startCoordinate) = let c = fromJust . moveTachyonDown cmap $ startCoordinate in length (updateSplitters cmap [] c)


-- Get a Coordinate (current position of the beam), a list of splitters, and return an updated list of splitters.
-- We don't need a state, since we don't output anything other than the updated list.
updateSplitters :: CMap Char -> [Coordinate] -> Coordinate -> [Coordinate]
updateSplitters cmap splitters c = if c `elem` splitters then splitters else splittersLeftAndRight where
    newTachyon s dir = maybe s (updateSplitters cmap s) . moveTachyonDown cmap . move dir $ c
    splittersLeftAndRight = let splittersLeft = newTachyon (c : splitters) DLeft in newTachyon splittersLeft DRight

moveTachyonDown :: CMap Char -> Coordinate -> Maybe Coordinate
moveTachyonDown cmap = moveDown where
    moveDown c = case lookup c cmap of
        Nothing  -> Nothing                             -- Out of the screen: Nothing
        Just '^' -> Just c                              -- Splitter: BINGO, return the coordinate
        _        -> moveDown (move DDown c)             -- Empty space or S: keep moving down
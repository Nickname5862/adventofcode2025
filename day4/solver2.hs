module Solver where
import Data.Maybe (isJust)
import Data.Map (Map, lookup, keys, filterWithKey)
import Prelude hiding (lookup)
import Utils (Coordinate, adjacent8, toCoordinateMapExcluding)
import Debug.Trace (traceShow)

type ParsedInput = Map Coordinate Char

parseInput :: String -> ParsedInput
parseInput = toCoordinateMapExcluding ['.']

solveInput :: ParsedInput -> Int
solveInput floormap = (sum . map snd . takeWhile ((/= 0) . snd) . drop 1) allSteps where -- drop the initial (floormap, 0), takeWhile we can still access rolls, and return the result
    allSteps = iterate (\(oldMap, _) -> let newStep = solveStep oldMap in traceShow (snd newStep) newStep) (floormap, 0) -- iterate forever

-- return the updated map and the number of rolls accessed
solveStep :: ParsedInput -> (ParsedInput, Int)
solveStep floormap = (updatedFloormap, length accessibleRolls) where
    accessibleRolls = filter (canBeAccessed floormap) (keys floormap)                  -- which rolls can be accessed?
    updatedFloormap = filterWithKey (\key _ -> key `notElem` accessibleRolls) floormap -- update the floormap by removing those rolls

canBeAccessed :: Map Coordinate Char -> Coordinate -> Bool
canBeAccessed floormap pos = (<4) $ length $ filter (isJust . (`lookup` floormap) . (+ pos)) adjacent8

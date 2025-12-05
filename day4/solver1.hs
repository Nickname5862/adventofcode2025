module Solver where
import Data.Maybe (isJust)
import Data.Map (Map, lookup, keys)
import Prelude hiding (lookup)
import Utils (Coordinate, adjacent8, toCoordinateMapExcluding)

type ParsedInput = Map Coordinate Char

parseInput :: String -> ParsedInput
parseInput = toCoordinateMapExcluding ['.']

solveInput :: ParsedInput -> Int
solveInput floormap = length $ filter (canBeAccessed floormap) (keys floormap)


canBeAccessed :: Map Coordinate Char -> Coordinate -> Bool
canBeAccessed floormap pos = (<4) $ length $ filter (isJust . (`lookup` floormap) . (+ pos)) adjacent8

module Day5 where

import Util
import Grid
import Data.List
import Data.List.Split
import Debug.Trace

type ItemName = String
type ItemNumber = Int
data GardenMap = GardenMap { destinationRangeStart :: ItemNumber, sourceRangeStart :: ItemNumber, rangeLength :: Int } deriving (Show, Eq)
data GardenConversion = GardenConversion { maps :: [GardenMap] } deriving (Show, Eq)
data Garden = Garden { conversions :: [GardenConversion] } deriving (Show, Eq)

parseGardenMap :: String -> GardenMap
parseGardenMap line = GardenMap d s r
  where
    [d, s, r] = readInts line

parseInput :: [String] -> (Garden, [Int])
parseInput lines = (Garden conversions, seeds)
  where
    [seedLine] : conversionBlocks = splitOn [""] lines
    seeds = readInts seedLine
    conversions = map parseConversion conversionBlocks
    parseConversion lines = GardenConversion maps
      where
        maps = map parseGardenMap $ tail lines

convertOnce :: GardenConversion -> ItemNumber -> ItemNumber
convertOnce conversion inputNumber = outputNumber
  where
    correctMap = find (\(GardenMap _ source range) -> source <= inputNumber && inputNumber < (source + range)) $ maps conversion
    outputNumber = case correctMap of
      Just (GardenMap dest source _) -> inputNumber - source + dest
      Nothing -> inputNumber

convertToLocation :: Garden -> Int -> ItemNumber -> ItemNumber
convertToLocation _ 6 n = n
convertToLocation garden conversionNr item = convertToLocation garden (conversionNr + 1) $ convertOnce (conversions garden !! conversionNr) item

day5 :: Foldable t => ([Int] -> t Int) -> [String] -> Int
day5 seedIterator input = foldl foldFunction (convert (head seedInput)) seeds
  where
    (garden, seedInput) = parseInput input
    Garden conversions = garden
    seeds = seedIterator seedInput
    convert = convertToLocation garden 0
    foldFunction mem one = minimum [mem, convert one]
    
day5a :: [String] -> Int
day5a = day5 id
  
day5b :: [String] -> Int
day5b = day5 seedIterator
  where
    seedIterator ints = concatMap expand $ chunksOf 2 ints
      where
        expand [nr, 1] = [nr]
        expand [nr, range] = nr : expand [nr + 1, range - 1]

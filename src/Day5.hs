module Day5 where

import Util
import Grid
import Data.List
import Data.List.Split
import Debug.Trace

type ItemName = String
type ItemNumber = Int
data GardenMap = GardenMap { destinationRangeStart :: ItemNumber, sourceRangeStart :: ItemNumber, rangeLength :: Int } deriving (Show, Eq)
data GardenConversion = GardenConversion { inputName :: ItemName, outputName :: ItemName, maps :: [GardenMap] } deriving (Show, Eq)
data Garden = Garden { seeds :: [ItemNumber], conversions :: [GardenConversion] } deriving (Show, Eq)
data Item = Item { name :: ItemName, number :: ItemNumber } deriving (Show, Eq)
type ParseSeeds = String -> [ItemNumber]

parseGardenMap :: String -> GardenMap
parseGardenMap line = GardenMap d s r
  where
    [d, s, r] = readInts line

parseInput :: ParseSeeds -> [String] -> Garden
parseInput parseSeeds lines = Garden seeds conversions
  where
    [seedLine] : conversionBlocks = splitOn [""] lines
    seeds = parseSeeds seedLine
    conversions = map parseConversion conversionBlocks
    parseConversion (firstLine : mapLines) = GardenConversion input output maps
      where
        [input, output] = splitOn "-to-" $ reverse $ drop 5 $ reverse firstLine
        maps = map parseGardenMap mapLines

convertOnce :: Garden -> Item -> Item
convertOnce (Garden _ conversions) (Item name inputNumber) = Item (outputName conversion) outputNumber
  where
    Just conversion = find (\c -> inputName c == name) conversions
    correctMap = find (\(GardenMap dest source range) -> source <= inputNumber && inputNumber < (source + range)) $ maps conversion
    outputNumber = case correctMap of
      Just (GardenMap dest source _) -> inputNumber - source + dest
      Nothing -> inputNumber

convertToLocation :: Garden -> Item -> ItemNumber
convertToLocation _ (Item "location" n) = n
convertToLocation garden item = convertToLocation garden $ convertOnce garden item

day5a :: [String] -> Int
day5a input = minimum $ map (convertToLocation garden) seeds
  where
    parseSeeds = readInts
    garden = parseInput parseSeeds input
    Garden seedNumbers conversions = garden
    seeds = map (Item "seed") seedNumbers
    
day5b :: [String] -> Int
day5b input = minimum $ map (convertToLocation garden) seeds
  where
    parseSeeds line = concatMap expand $ chunksOf 2 $ readInts line
      where
        expand [nr, 1] = [nr]
        expand [nr, range] = nr : expand [nr + 1, range - 1]
    garden = parseInput parseSeeds input
    Garden seedNumbers conversions = garden
    seeds = map (Item "seed") seedNumbers

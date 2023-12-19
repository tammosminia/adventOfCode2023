module Day6 where

import qualified Util
import Grid
import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Text as Text

type Time = Int
type Distance = Int
type Race = (Time, Distance)

parseInput :: [String] -> [Race]
parseInput [timeLine, distanceLine] = zip times distances 
  where
    times = Util.readInts timeLine
    distances = Util.readInts distanceLine

distanceTravelled :: Time -> Time -> Distance
distanceTravelled timePressed totalTime = timePressed * (totalTime - timePressed)

ways :: Race -> Int
ways (time, distance) = Util.count wins [1..(time-1)]
  where
    wins timePressed = distanceTravelled timePressed time > distance

day6a :: [String] -> Int
day6a input = product $ map ways $ parseInput input

day6b :: [String] -> Int
day6b input = ways $ parseInputB input
  where
    parseInputB [timeLine, distanceLine] = (parseLine timeLine, parseLine distanceLine)
    parseLine line = head $ Util.readInts $ Text.unpack $ Text.replace (Text.pack " ") (Text.pack "") (Text.pack line)

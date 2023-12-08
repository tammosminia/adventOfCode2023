module Day2 where

import Util
import Data.List
import Data.List.Split
import Data.Maybe

data Cubes = Cubes { red :: Int, green :: Int, blue :: Int }
data Game = Game { gameId :: Int, samples :: [Cubes] }

parseGame :: String -> Game
parseGame line = Game id samples
  where
    [gameS, samplesS] = splitOn ":" line
    id = head $ readInts gameS
    samples = map parseSample $ splitOn ";" samplesS

parseSample :: String -> Cubes
parseSample s = Cubes (amountOf "red") (amountOf "green") (amountOf "blue")
  where 
    colorStrings = splitOn "," s
    amountOf colorName = head $ readInts $ fromMaybe "0" (find (isColor colorName) colorStrings)
    isColor colorName colorString = isInfixOf colorName colorString

day2a :: [String] -> Int
day2a input = sum $ map gameId $ filter isPossibleGame games
  where
    games = map parseGame input
    isPossibleGame (Game id samples) = all isPossibleCubes samples
    isPossibleCubes (Cubes r g b) = r <= 12 && g <= 13 && b <= 14

day2b :: [String] -> Int
day2b input = sum $ map (gamePower . parseGame) input

gamePower :: Game -> Int
gamePower (Game id samples) = minRed * minGreen * minBlue
  where
    minRed = maximum $ map red samples
    minGreen = maximum $ map green samples
    minBlue = maximum $ map blue samples
    
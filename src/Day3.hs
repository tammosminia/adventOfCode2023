module Day3 where

import Util
import Grid
import Data.List

data Parsed = Number { left :: Point, right :: Point, number :: Int } | Symbol { location :: Point, symbol :: Char } deriving (Show, Eq)

  
parse :: [String] -> [Parsed]
parse input = concatMap (\(i, line) -> parseLine line (0, i)) $ zip [0..] input
    
parseLine :: String -> Point -> [Parsed]    
parseLine [] _ = []
parseLine (h:t) (x, y)
  | isDigit h = parseNumber (x, y) [h] t (x + 1, y)
  | h == '.' = parseLine t (x + 1, y)
  | otherwise = Symbol (x, y) h : parseLine t (x + 1, y)
  where
    parseNumber left cn [] (x, y) = [Number left (lx + length cn - 1, ly) (stringToInt cn)]
      where
        (lx, ly) = left
    parseNumber left cn (h:t) (x, y)
      | isDigit h = parseNumber left (cn ++ [h]) t (x + 1, y)
      | otherwise = Number left (lx + length cn - 1, ly) (stringToInt cn) : parseLine (h:t) (x, y)
        where
          (lx, ly) = left

isNumber :: Parsed -> Bool
isNumber (Number {}) = True
isNumber _ = False
    
day3a :: [String] -> Int
day3a input = sum $ map number $ filter isNextToSymbol numbers
  where
    (numbers, symbols) = partition isNumber $ parse input
    isNextToSymbol number = any (adjacent number) symbols
    adjacent (Number (lx, ly) (rx, ry) _) (Symbol (x, y) _) = lx - 1 <= x && x <= rx + 1 && ly - 1 <= y && y <= ry + 1
    

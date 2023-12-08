module Day1 where

import Util
import Data.List
import Data.Text (replace, pack, unpack)

day1a :: [String] -> Int
day1a input = sum $ map calcLine input
  where
    calcLine line = 10 * head digits + last digits
      where
        digits = readDigits line

day1b :: [String] -> Int
day1b input = day1a $ map convertWords input
  where
    translations = [("one", "on1e"), ("two", "tw2o"), ("three", "thr3ee"), ("four", "f4our"), 
                    ("five", "fi5ve"), ("six", "s6ix"), ("seven", "sev7en"),
                    ("eight", "eig8ht"), ("nine", "ni9ne")]
    convertWords line = foldl (\acc (k,v) -> findAndReplace k v acc) line translations

findAndReplace :: String -> String -> String -> String
findAndReplace target replacement base = 
  unpack $ Data.Text.replace (pack target) (pack replacement) (pack base)
  
  
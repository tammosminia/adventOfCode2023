module Day1 where

import Util
import Data.List
  
day1a :: [String] -> Int
day1a input = sum $ map calcLine input
  where
    calcLine line = 10 * head digits + last digits
      where
        digits = readDigits line
    
--day1b :: [[Integer]] -> Integer
--day1b input = sum $ take 3 $ reverse $ sort perElf
--  where
--    perElf = map sum input
    

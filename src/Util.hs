module Util where

import Data.List
import qualified Data.Map as Map
import Text.Regex.Base
import Text.Regex.Posix

mapOfLists :: (Ord k, Eq k) => [(k, v)] -> Map.Map k [v]
mapOfLists l = Map.fromList lll
  where
    ll = groupBy (\(k1, v1) (k2, v2) -> k1 == k2) $ sortOn fst l
    lll = map (\sl -> (fst (head sl), map snd sl)) ll

countMap :: (Ord k, Eq k) => [k] -> Map.Map k Int
countMap l = Map.fromList $ map count $ group $ sort l
  where count l = (head l, length l)

mergeMaps :: (Ord k, Eq k, Num v) => [Map.Map k v] -> Map.Map k v
mergeMaps = foldr (Map.unionWith (+)) Map.empty

replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs
   
charToInt :: Char -> Int
charToInt c = read [c] :: Int

stringToInt :: String -> Int
stringToInt c = read c :: Int

stringToInteger :: String -> Integer
stringToInteger c = read c :: Integer

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xz@(x:xs)
  | length v < n = []
  | otherwise = v : slidingWindow n xs
  where
    v = take n xz

-- return all combinations of i elements (only one way)
combinations :: Int -> [a] -> [[a]]
combinations 1 [] = []
combinations 1 (h : t) = [h] : combinations 1 t
combinations i [] = []
combinations i (h : t) = rh ++ rt
  where
    rh = map (\x -> (h : x)) (lesser t)
    lesser l = combinations (i - 1) l
    rt = combinations i t

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil pred [] = []
takeUntil pred (x:xs) 
  | pred x = [x]
  | otherwise = (x:takeUntil pred xs)
  
readInts :: String -> [Int]
readInts s = map stringToInt intStrs
  where
    intStrs = map head (s =~ "[0-9]+" :: [[String]])

readDigits :: String -> [Int]
readDigits = map charToInt . filter isDigit
    
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
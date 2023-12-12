module Day4 where

import Util
import Grid
import Data.List
import Data.List.Split
import Debug.Trace

data Card = Card { nr :: Int, win :: [Int], have :: [Int] }

parseCard :: String -> Card
parseCard line = Card (head $ readInts gameS) (readInts winS) (readInts haveS)
  where
    [gameS, afterGameS] = splitOn ":" line
    [winS, haveS] = splitOn "|" afterGameS
    
cardMatches :: Card -> Int
cardMatches (Card _ wins haves) = count (\x -> elem x wins) haves

cardPoints :: Card -> Int
cardPoints card = points $ cardMatches card
  where
    points 0 = 0
    points 1 = 1
    points x = 2 * points (x - 1)

day4a :: [String] -> Int
day4a input = sum $ map (cardPoints . parseCard) input

day4b :: [String] -> Int
day4b input = sum $ calculate startAmounts 1
  where
    dummyCard = Card 0 [] []
    cards = dummyCard : map parseCard input
    startAmounts = 0 : map (const 1) input
    calculate amounts currentCardNr
      | currentCardNr == length cards = amounts
      | otherwise = calculate newAmounts (currentCardNr + 1)
        where 
          newAmounts = addPoints amounts currentCardNr (amounts !! currentCardNr) $ cardMatches (cards !! currentCardNr)
    addPoints amounts _ _ 0 = amounts
    addPoints amounts currentCardNr times points = addPoints newAmounts nextCard times (points - 1)
      where
        nextCard = currentCardNr + 1
        newAmounts = replace amounts nextCard ((amounts !! nextCard) + times) 
          

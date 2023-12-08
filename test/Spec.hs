import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.Map as Map

import Util
import qualified Grid
import Day1
import Day2

main :: IO ()
main = hspec $ do
  describe "util" $ do
    it "combinations" $ do
      combinations 1 [1] `shouldBe` [[1]]
      combinations 1 [1,2] `shouldBe` [[1], [2]]
      combinations 1 [1,2,3] `shouldBe` [[1], [2], [3]]

      combinations 2 [1] `shouldBe` []
      combinations 2 [1,2] `shouldBe` [[1,2]]
      combinations 2 [1,2,3] `shouldBe` [[1,2], [1,3], [2,3]]

      combinations 3 [1] `shouldBe` []
      combinations 3 [1,2] `shouldBe` []
      combinations 3 [1,2,3] `shouldBe` [[1,2,3]]
      combinations 3 [1,2,3,4] `shouldBe` [[1,2,3], [1,2,4], [1,3,4], [2,3,4]]

    it "count" $ do
      count (>2) [1,2,3,4,5] `shouldBe` 3
      
    it "takeUntil" $ do
      takeUntil (>2) [1,2,3,4,5] `shouldBe` [1,2,3]
      
    it "readInts" $ do
      readInts "" `shouldBe` []
      readInts "bla" `shouldBe` []
      readInts "13" `shouldBe` [13]
      readInts "bla 1 2 bla 3" `shouldBe` [1,2,3]
      
  describe "grid" $ do
    it "allPoints" $ do
      Grid.allPoints (Grid.init ["12", "34"])  `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
      
  describe "day 1" $ do
    it "day1a" $ do
      day1a ["1abc2","pqr3stu8vwx","a1b2c3d4e5f","treb7uchet"] `shouldBe` 142
      day1a ["11a22"] `shouldBe` 12
      day1a ["09"] `shouldBe` 9
    it "day1b" $ do
      day1b ["twothreetwotwo"] `shouldBe` 22
      day1b ["two1nine"] `shouldBe` 29
      day1b ["eightwothree"] `shouldBe` 83
      day1b ["abcone2threexyz"] `shouldBe` 13
      day1b ["xtwone3four"] `shouldBe` 24
      day1b ["4nineeightseven2"] `shouldBe` 42
      day1b ["zoneight234"] `shouldBe` 14
      day1b ["7pqrstsixteen"] `shouldBe` 76
      day1b ["two1nine","eightwothree","abcone2threexyz","xtwone3four","4nineeightseven2","zoneight234","7pqrstsixteen"] `shouldBe` 281

  describe "day 2" $ do
    let example = ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green","Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue","Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red","Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red","Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]
    it "day2a" $ do
      day2a example `shouldBe` 8
    it "gamePower" $ do
      gamePower (parseGame (head example)) `shouldBe` 48
    it "day2b" $ do
      day2b example `shouldBe` 2286

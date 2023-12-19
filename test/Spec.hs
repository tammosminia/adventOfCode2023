import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.Map as Map

import Util
import qualified Grid
import Day1
import Day2
import Day3
import Day4
import Day5

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
      
  describe "day 3" $ do
    let example = ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]
    it "parse" $ do
      parse example `shouldBe` [Number (0,0) (2,0) 467, Number (5,0) (7,0) 114, Symbol (3,1) '*', Number (2,2) (3,2) 35, Number (6,2) (8,2) 633, Symbol (6,3) '#', Number (0,4) (2,4) 617, Symbol (3,4) '*', Symbol (5,5) '+', Number (7,5) (8,5) 58, Number (2,6) (4,6) 592, Number (6,7) (8,7) 755, Symbol (3,8) '$', Symbol (5,8) '*', Number (1,9) (3,9) 664, Number (5,9) (7,9) 598]
      parse ["...123"] `shouldBe` [Number (3,0) (5,0) 123]
    it "day3a" $ do
      day3a example `shouldBe` 4361
    it "day3b" $ do
      day3b example `shouldBe` 467835
      
  describe "day 4" $ do
    let example = ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53","Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19","Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1","Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83","Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36","Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
    it "day4a" $ do
      day4a example `shouldBe` 13
    it "day4b" $ do
      day4b example `shouldBe` 30
      
  describe "day 5" $ do
    let example = ["seeds: 79 14 55 13","","seed-to-soil map:","50 98 2","52 50 48","","soil-to-fertilizer map:","0 15 37","37 52 2","39 0 15","","fertilizer-to-water map:","49 53 8","0 11 42","42 0 7","57 7 4","","water-to-light map:","88 18 7","18 25 70","","light-to-temperature map:","45 77 23","81 45 19","68 64 13","","temperature-to-humidity map:","0 69 1","1 0 69","","humidity-to-location map:","60 56 37","56 93 4"]
    it "day5a" $ do
      day5a example `shouldBe` 35
    it "day5b" $ do
      day5b example `shouldBe` 46
      
      
      

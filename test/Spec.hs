import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.Set as Set
import Data.Map.Strict as Map
import Data.List as List

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import Day4 (Log(..))
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True} specs

specs :: Spec
specs =
    describe "Advent of Code" $ do
      describe "day 1" $ do
        it "returns resulting frequency" $ do
          Day1.solution1 [1 , 2 , -1] `shouldBe` 2
        it "returns first duplicate frequency" $ do
          Day1.solution2 [ 3, 3, 4, -2, -4] `shouldBe` 10
      describe "day 2" $ do
        it "returns checksum" $ do
          Day2.solution1 [ "abcdef" , "bababc" , "abbcde" , "abcccd" , "aabcdd" , "abcdee" , "ababab"] `shouldBe` 12
        it "returns common letters of two similar strings" $ do
          Day2.solution2 [ "abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"] `shouldBe` "fgij"
      describe "day 3" $ do
        it "returns number of overlapping points" $ do
          Day3.solution1 [ Day3.Rectangle 1 1 3 4 4, Day3.Rectangle 2 3 1 4 4, Day3.Rectangle 3 5 5 2 2] `shouldBe` 4
        it "returns id of first non overlapping rectangle" $ do
          Day3.solution2 [ Day3.Rectangle 1 1 3 4 4, Day3.Rectangle 2 3 1 4 4, Day3.Rectangle 3 5 5 2 2] `shouldBe` 3
      describe "day 4" $ do
        it "returns id of the most sleepy guard multiplied by number of minute" $ do
          Day4.solution1 [BeginShift 1 3, FallAsleep 2 10, WakeUp 3 30,
                          BeginShift 4 2, FallAsleep 5 10, WakeUp 6 50,
                          BeginShift 7 3, FallAsleep 8 20, WakeUp 9 21] `shouldBe` 20 -- 2 * 20
        it "returns id of the most frequently asleep guard multiplied by number of minute" $ do
          Day4.solution2 [BeginShift 1 3, FallAsleep 2 18, WakeUp 3 25,
                          BeginShift 4 2, FallAsleep 5 10, WakeUp 6 50,
                          BeginShift 7 3, FallAsleep 8 20, WakeUp 9 21] `shouldBe` 60 -- 3 * 20
      describe "day 5" $ do
        it "returns number of elements left after alchemical reduction" $ do
          Day5.solution1 "dabAcCaCBAcCcaDA" `shouldBe` 10 -- dabCBAcaDA
        it "returns number of elements left after alchemical reduction with one element type removed" $ do
          Day5.solution2 "dabAcCaCBAcCcaDA" `shouldBe` 4 -- daDA
      describe "day 6" $ do
        it "returns size or the largest finite area" $ do
          Day6.solution1 [(1, 1),(1, 6),(8, 3),(3, 4),(5, 5),(8, 9)] `shouldBe` 17 -- (5,5)
        it "returns size of region near as many coordinates as possible" $ do
          Day6.solution2 [(1, 1),(1, 6),(8, 3),(3, 4),(5, 5),(8, 9)] 32 `shouldBe` 16
      describe "day 7" $ do
        it "returns steps in correct order" $ do
          Day7.solution1 [('C', 'A'), ('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')] `shouldBe` "CABDFE"
        it "returns how long it takes to finish all steps" $ do
          Day7.solution2 [('C', 'A'), ('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')] 2 0 `shouldBe` 15
      describe "day 8" $ do
        it "returns the sum of all metadata" $ do
          Day8.solution1 [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2] `shouldBe` 138
        it "returns value of the tree node" $ do
          Day8.solution2 [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2] `shouldBe` 66
      describe "day 9" $ do
        it "returns the highest score" $ do
          Day9.solution1 9 25 `shouldBe` 32
          Day9.solution1 10 1618 `shouldBe` 8317
          Day9.solution1 13 7999 `shouldBe` 146373
          Day9.solution1 17 1104 `shouldBe` 2764
          Day9.solution1 21 6111 `shouldBe` 54718
          Day9.solution1 30 5807 `shouldBe` 37305
      describe "day 10" $ do
        it "returns points when they cover the smallest area" $ do
          Day10.solution1 [((1,1),(1,1)), ((-1,1),(5,1)), ((1,-1),(1,5)), ((-1,-1),(5,5))] `shouldBe` Set.fromList [(3,3)]
        it "returns time it would take for message to appear" $ do
          Day10.solution2 [((1,1),(1,1)), ((-1,1),(5,1)), ((1,-1),(1,5)), ((-1,-1),(5,5))] `shouldBe` 2
      describe "day 11" $ do
        it "grid contains cells with correct power level" $ do
          Day11.grid 8 ! (3,5) `shouldBe` 4
          Day11.grid 57 ! (122,79) `shouldBe` (-5)
          Day11.grid 39 ! (217,196) `shouldBe` 0
          Day11.grid 71 ! (101,153) `shouldBe` 4
        it "returns coordinates of square with the highest power level" $ do
          Day11.solution1 18 `shouldBe` (33,45)
          Day11.solution1 42 `shouldBe` (21,61)
        it "returns the largest square" $ do
          Day11.solution2 18 `shouldBe` (90,269,16)
          Day11.solution2 42 `shouldBe` (232,251,12)

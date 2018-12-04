import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True} specs

specs :: Spec
specs =
    describe "Advent of Code" $ do
      describe "day 1" $ do
        it "returns sum" $ do
          Day1.solution1 [1 , 2 , -1] `shouldBe` 2
        it "returns first duplicate accumulator value" $ do
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
          Day4.solution1 [Day4.BeginShift 1 3, Day4.FallsAsleep 2 10, Day4.WakesUp 3 30, Day4.BeginShift 4 2, Day4.FallsAsleep 5 10, Day4.WakesUp 6 10] `shouldBe` 30

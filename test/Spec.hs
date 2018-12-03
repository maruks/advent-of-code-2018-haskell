import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import qualified Day1
import qualified Day2

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
        it "returns commonn letters of two similar strings" $ do
          Day2.solution2 [ "abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"] `shouldBe` "fgij"

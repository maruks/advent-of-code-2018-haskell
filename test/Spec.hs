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
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import Day15 (Point(..), Unit(..), Square(..))

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
      -- describe "day 11" $ do
      --   it "grid contains cells with correct power level" $ do
      --     Day11.grid 8 ! (3,5) `shouldBe` 4
      --     Day11.grid 57 ! (122,79) `shouldBe` (-5)
      --     Day11.grid 39 ! (217,196) `shouldBe` 0
      --     Day11.grid 71 ! (101,153) `shouldBe` 4
      --   it "returns coordinates of square with the highest power level" $ do
      --     Day11.solution1 18 `shouldBe` (33,45)
      --     Day11.solution1 42 `shouldBe` (21,61)
      --   it "returns the largest square" $ do
      --     Day11.solution2 18 `shouldBe` (90,269,16)
      --     Day11.solution2 42 `shouldBe` (232,251,12)
      -- describe "day 12" $ do
      --   let initialState = [False,False,False,True,True,False,False,False]
      --   it "grow plants returns plant configuration after specified number of iterations" $ do
      --     Day12.growPlants initialState 1 Map.empty `shouldBe` [False,False,False,False,False,False,False,False,False,False]
      --     Day12.growPlants initialState 1 (Map.fromList [((False,False,False,True,True),True),((True,True,False,False,False),True)]) `shouldBe` [False,False,False,True,False,False,True,False,False,False]
      -- describe "day 13" $ do
      --   it "returns location of the first crash" $ do
      --     file <- readFile "./test/day13-part1-example.txt"
      --     let (carts,tracks) = Day13.readInput $ lines file
      --     Day13.solution1 tracks carts `shouldBe` (7,3)
      --   it "returns location of the last cart" $ do
      --     file <- readFile "./test/day13-part2-example.txt"
      --     let (carts,tracks) = Day13.readInput $ lines file
      --     Day13.solution2 tracks carts `shouldBe` (6,4)
      -- describe "day 14" $ do
      --   it "returns the scores of the ten recipes after the first N on the scoreboard" $ do
      --     Day14.solution1 9 `shouldBe` [5,1,5,8,9,1,6,7,7,9]
      --     Day14.solution1 5 `shouldBe` [0,1,2,4,5,1,5,8,9,1]
      --     Day14.solution1 18 `shouldBe` [9,2,5,1,0,7,1,0,8,5]
      --     Day14.solution1 2018 `shouldBe` [5,9,4,1,4,2,9,8,8,2]
      --   it "returns number of recipes produced when specified score appears on the chocolate scoreboard" $ do
      --     Day14.solution2 [5,9,4,1,4] `shouldBe` 2018
      --     Day14.solution2 [9,2,5,1,0] `shouldBe` 18
      --     Day14.solution2 [5,1,5,8,9] `shouldBe` 9
      --     Day14.solution2 [0,1,2,4,5] `shouldBe` 5
      describe "day 15" $ do
        let map0 = Day15.buildMap ["#######",
                                   "#E..G.#",
                                   "#...#.#",
                                   "#.G.#G#",
                                   "#######"]

            map1 = Day15.buildMap ["#######",
                                   "#.G...#",
                                   "#...EG#",
                                   "#.#.#G#",
                                   "#..G#E#",
                                   "#.....#",
                                   "#######"]

            map2 = Day15.buildMap ["#######",
                                   "#G..#E#",
                                   "#E#E.E#",
                                   "#G.##.#",
                                   "#...#E#",
                                   "#...E.#",
                                   "#######"]

            map3 = Day15.buildMap ["#######",
                                   "#E..EG#",
                                   "#.#G.E#",
                                   "#E.##E#",
                                   "#G..#.#",
                                   "#..E#.#",
                                   "#######"]

            map4 = Day15.buildMap ["#######",
                                   "#E.G#.#",
                                   "#.#G..#",
                                   "#G.#.G#",
                                   "#G..#.#",
                                   "#...E.#",
                                   "#######"]

            map5 = Day15.buildMap ["#######",
                                   "#.E...#",
                                   "#.#..G#",
                                   "#.###.#",
                                   "#E#G#G#",
                                   "#...#G#",
                                   "#######"]

            map6 = Day15.buildMap ["#########",
                                   "#G......#",
                                   "#.E.#...#",
                                   "#..##..G#",
                                   "#...##..#",
                                   "#...#...#",
                                   "#.G...G.#",
                                   "#.....G.#",
                                   "#########"]

            map7 = Day15.buildMap ["####",
                                   "##E#",
                                   "#GG#",
                                   "####"]

            map8 = Day15.buildMap ["#####",
                                   "#GG##",
                                   "#.###",
                                   "#..E#",
                                   "#.#G#",
                                   "#.E##",
                                   "#####"]

        it "returns points to targets" $ do
          let targets = [Point {x = 3, y = 1},
                         Point {x = 5, y = 1},
                         Point {x = 2, y = 2},
                         Point {x = 5, y = 2},
                         Point {x = 1, y = 3},
                         Point {x = 3, y = 3}]
              points = Day15.pointsToTargets (Elf 200 "") (Point 1 1) map0
          List.sort points `shouldBe` targets
        it "returns shortest path to the nearest target" $ do
          let targets1 = Day15.pointsToTargets (Elf 200 "") (Point 1 1) map0
          let targets2 = Day15.pointsToTargets (Goblin 200 "") (Point 2 3) map0
          Day15.shortestPath (Point 1 1) targets1 map0 `shouldBe` [Point 2 1,Point 3 1]
          Day15.shortestPath (Point 2 3) targets2 map0 `shouldBe` [Point 2 2,Point 2 1]
        it "returns empty path if none of targets can be reached" $ do
          let targets = Day15.pointsToTargets (Goblin 200 "") (Point 5 3) map0
          Day15.shortestPath (Point 5 3) targets map0 `shouldBe` []
        it "returns outcome of the combat" $ do
          Day15.solution1 map1 `shouldBe` 27730
          Day15.solution1 map2 `shouldBe` 36334
          Day15.solution1 map3 `shouldBe` 39514
          Day15.solution1 map4 `shouldBe` 27755
          Day15.solution1 map5 `shouldBe` 28944
          Day15.solution1 map6 `shouldBe` 18740
      --    Day15.solution1 map7 `shouldBe` 13400
          Day15.solution1 map8 `shouldBe` 13987
        it "returns outcome of the combat with increased attack power of elves" $ do
          Day15.solution2 map1 `shouldBe` 4988
          Day15.solution2 map3 `shouldBe` 31284
          Day15.solution2 map4 `shouldBe` 3478
          Day15.solution2 map5 `shouldBe` 6474
          Day15.solution2 map6 `shouldBe` 1140

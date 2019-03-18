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
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day22
import qualified Day23
import qualified Day24
import Day24 (Damage (..), Team (..), Group (..))
import qualified Day25

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True} specs

specs :: Spec
specs =
    describe "Advent of Code" $ do
      describe "day 1" $ do
        it "returns resulting frequency" $ 
          Day1.solution1 [1 , 2 , -1] `shouldBe` 2
        it "returns first duplicate frequency" $ 
          Day1.solution2 [ 3, 3, 4, -2, -4] `shouldBe` 10
      describe "day 2" $ do
        it "returns checksum" $ 
          Day2.solution1 [ "abcdef" , "bababc" , "abbcde" , "abcccd" , "aabcdd" , "abcdee" , "ababab"] `shouldBe` 12
        it "returns common letters of two similar strings" $ 
          Day2.solution2 [ "abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"] `shouldBe` "fgij"
      describe "day 3" $ do
        it "returns number of overlapping points" $ 
          Day3.solution1 [ Day3.Rectangle 1 1 3 4 4, Day3.Rectangle 2 3 1 4 4, Day3.Rectangle 3 5 5 2 2] `shouldBe` 4
        it "returns id of first non overlapping rectangle" $ 
          Day3.solution2 [ Day3.Rectangle 1 1 3 4 4, Day3.Rectangle 2 3 1 4 4, Day3.Rectangle 3 5 5 2 2] `shouldBe` 3
      describe "day 4" $ do
        it "returns id of the most sleepy guard multiplied by number of minute" $ 
          Day4.solution1 [BeginShift 1 3, FallAsleep 2 10, WakeUp 3 30,
                          BeginShift 4 2, FallAsleep 5 10, WakeUp 6 50,
                          BeginShift 7 3, FallAsleep 8 20, WakeUp 9 21] `shouldBe` 20 -- 2 * 20
        it "returns id of the most frequently asleep guard multiplied by number of minute" $ 
          Day4.solution2 [BeginShift 1 3, FallAsleep 2 18, WakeUp 3 25,
                          BeginShift 4 2, FallAsleep 5 10, WakeUp 6 50,
                          BeginShift 7 3, FallAsleep 8 20, WakeUp 9 21] `shouldBe` 60 -- 3 * 20
      describe "day 5" $ do
        it "returns number of elements left after alchemical reduction" $ 
          Day5.solution1 "dabAcCaCBAcCcaDA" `shouldBe` 10 -- dabCBAcaDA
        it "returns number of elements left after alchemical reduction with one element type removed" $ 
          Day5.solution2 "dabAcCaCBAcCcaDA" `shouldBe` 4 -- daDA
      describe "day 6" $ do
        it "returns size or the largest finite area" $ 
          Day6.solution1 [(1, 1),(1, 6),(8, 3),(3, 4),(5, 5),(8, 9)] `shouldBe` 17 -- (5,5)
        it "returns size of region near as many coordinates as possible" $ 
          Day6.solution2 [(1, 1),(1, 6),(8, 3),(3, 4),(5, 5),(8, 9)] 32 `shouldBe` 16
      describe "day 7" $ do
        it "returns steps in correct order" $ 
          Day7.solution1 [('C', 'A'), ('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')] `shouldBe` "CABDFE"
        it "returns how long it takes to finish all steps" $ 
          Day7.solution2 [('C', 'A'), ('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')] 2 0 `shouldBe` 15
      describe "day 8" $ do
        it "returns the sum of all metadata" $ 
          Day8.solution1 [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2] `shouldBe` 138
        it "returns value of the tree node" $ 
          Day8.solution2 [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2] `shouldBe` 66
      describe "day 9" $ 
        it "returns the highest score" $ do
          Day9.solution1 9 25 `shouldBe` 32
          Day9.solution1 10 1618 `shouldBe` 8317
          Day9.solution1 13 7999 `shouldBe` 146373
          Day9.solution1 17 1104 `shouldBe` 2764
          Day9.solution1 21 6111 `shouldBe` 54718
          Day9.solution1 30 5807 `shouldBe` 37305
      describe "day 10" $ do
        it "returns points when they cover the smallest area" $ 
          Day10.solution1 [((1,1),(1,1)), ((-1,1),(5,1)), ((1,-1),(1,5)), ((-1,-1),(5,5))] `shouldBe` Set.fromList [(3,3)]
        it "returns time it would take for message to appear" $ 
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
      describe "day 12" $ do
        let initialState = [False,False,False,True,True,False,False,False]
        it "grow plants returns plant configuration after specified number of iterations" $ do
          Day12.growPlants initialState 1 Map.empty `shouldBe` [False,False,False,False,False,False,False,False,False,False]
          Day12.growPlants initialState 1 (Map.fromList [((False,False,False,True,True),True),((True,True,False,False,False),True)]) `shouldBe` [False,False,False,True,False,False,True,False,False,False]
      describe "day 13" $ do
        it "returns location of the first crash" $ do
          file <- readFile "./test/day13-part1-example.txt"
          let (carts,tracks) = Day13.readInput $ lines file
          Day13.solution1 tracks carts `shouldBe` (7,3)
        it "returns location of the last cart" $ do
          file <- readFile "./test/day13-part2-example.txt"
          let (carts,tracks) = Day13.readInput $ lines file
          Day13.solution2 tracks carts `shouldBe` (6,4)
      describe "day 14" $ do
        it "returns the scores of the ten recipes after the first N on the scoreboard" $ do
          Day14.solution1 9 `shouldBe` [5,1,5,8,9,1,6,7,7,9]
          Day14.solution1 5 `shouldBe` [0,1,2,4,5,1,5,8,9,1]
          Day14.solution1 18 `shouldBe` [9,2,5,1,0,7,1,0,8,5]
          Day14.solution1 2018 `shouldBe` [5,9,4,1,4,2,9,8,8,2]
        it "returns number of recipes produced when specified score appears on the chocolate scoreboard" $ do
          Day14.solution2 [5,9,4,1,4] `shouldBe` 2018
          Day14.solution2 [9,2,5,1,0] `shouldBe` 18
          Day14.solution2 [5,1,5,8,9] `shouldBe` 9
          Day14.solution2 [0,1,2,4,5] `shouldBe` 5
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

            map9 = [ "#################"
                   , "##..............#"
                   , "##........G.....#"
                   , "####.....#....###"
                   , "#....##......####"
                   , "#...............#"
                   , "##........##....#"
                   , "##.........E..#.#"
                   , "#####.###...#####"
                   , "#################" ]

            map10 = ["#################"
                   , "##..............#"
                   , "##..............#"
                   , "####.....#G...###"
                   , "#....##......####"
                   , "#...............#"
                   , "##........##....#"
                   , "##..........E.#.#"
                   , "#####.###...#####"
                   , "#################" ]

            map_1 =   ["#########",
                       "#G..G..G#",
                       "#.......#",
                       "#.......#",
                       "#G..E..G#",
                       "#.......#",
                       "#.......#",
                       "#G..G..G#",
                       "#########"]

            map_2 =  ["#########",
                      "#.G...G.#",
                      "#...G...#",
                      "#...E..G#",
                      "#.G.....#",
                      "#.......#",
                      "#G..G..G#",
                      "#.......#",
                      "#########"]

            map_3 =  ["#########",
                      "#..G.G..#",
                      "#...G...#",
                      "#.G.E.G.#",
                      "#.......#",
                      "#G..G..G#",
                      "#.......#",
                      "#.......#",
                      "#########"]

            map_4 =  ["#########",
                      "#.......#",
                      "#..GGG..#",
                      "#..GEG..#",
                      "#G..G...#",
                      "#......G#",
                      "#.......#",
                      "#.......#",
                      "#########"]

        it "returns points to targets" $ do
          let targets = [Point 3 1, Point 5 1, Point 2 2, Point 5 2, Point 1 3, Point 3 3]
              points = Day15.pointsToTargets (Elf 200 "") (Point 1 1) map0
          List.sort points `shouldBe` targets
        it "returns shortest path to the nearest target" $ do
          let targets1 = Day15.pointsToTargets (Elf 200 "") (Point 1 1) map0
              targets2 = Day15.pointsToTargets (Goblin 200 "") (Point 2 3) map0
              sp1 = Day15.shortestPath (Point 1 1) targets1 map0
              sp2 = Day15.shortestPath (Point 2 3) targets2 map0
          sp1 `shouldBe` Just (Point 2 1)
          sp2 `shouldBe` Just (Point 2 2)
        it "returns nothing if none of targets can be reached" $ do
          let targets = Day15.pointsToTargets (Goblin 200 "") (Point 5 3) map0
          Day15.shortestPath (Point 5 3) targets map0 `shouldBe` Nothing
        it "units move around" $ do
          let m2 = fst $ Day15.performRound (Day15.buildMap map_1) 3
              m3 = fst $ Day15.performRound (Day15.buildMap map_2) 3
              m4 = fst $ Day15.performRound (Day15.buildMap map_4) 3
          lines (Day15.printMap m2) `shouldBe` map_2
          lines (Day15.printMap m3) `shouldBe` map_3
          lines (Day15.printMap m4) `shouldBe` map_4
        it "goblin moves down in map 9" $ do
          let m9 = fst $ Day15.performRound (Day15.buildMap map9) 3
          lines (Day15.printMap m9) `shouldBe` map10
        it "returns outcome of the combat" $ do
          file <- readFile "test/day15.txt"
          file_1 <- readFile "test/day15-example-1.txt"
          let example1 = Day15.buildMap $ lines file_1
              day15map = Day15.buildMap $ lines file
          Day15.solution1 map1 `shouldBe` 27730
          Day15.solution1 map2 `shouldBe` 36334
          Day15.solution1 map3 `shouldBe` 39514
          Day15.solution1 map4 `shouldBe` 27755
          Day15.solution1 map5 `shouldBe` 28944
          Day15.solution1 map6 `shouldBe` 18740
          Day15.solution1 map7 `shouldBe` 13400
          Day15.solution1 map8 `shouldBe` 13987
          Day15.solution1 example1 `shouldBe` 215168
          Day15.solution1 day15map `shouldBe` 250594
        it "returns outcome of the combat with increased attack power of elves" $ do
          file <- readFile "test/day15.txt"
          file_0 <- readFile "test/day15-example-1.txt"
          file_1 <- readFile "test/day15-example-1-round-1.txt"
          file_17 <- readFile "test/day15-example-1-round-17.txt"
          file_42 <- readFile "test/day15-example-1-round-42.txt"
          let day15map = Day15.buildMap $ lines file
              example1 = Day15.buildMap $ lines file_0
              round_1_map = Day15.printMap $ Day15.buildMap $ lines file_1
              round_17_map = Day15.printMap $ Day15.buildMap $ lines file_17
              round_42_map = Day15.printMap $ Day15.buildMap $ lines file_42
              example_1_rounds = Day15.rounds example1 16
              round_1 = fst $ head example_1_rounds
              round_17 = fst (example_1_rounds !! 16)
              round_42 = fst (example_1_rounds !! 41)
          Day15.solution2 map1 `shouldBe` 4988
          Day15.solution2 map3 `shouldBe` 31284
          Day15.solution2 map4 `shouldBe` 3478
          Day15.solution2 map5 `shouldBe` 6474
          Day15.solution2 map6 `shouldBe` 1140
          Day15.solution2 example1 `shouldBe` 52374
          Day15.solution2 day15map `shouldBe` 52133
          Day15.printMap round_1 `shouldBe` round_1_map
          Day15.printMap round_17 `shouldBe` round_17_map
          Day15.printMap round_42 `shouldBe` round_42_map
      describe "day 16" $ 
        it "returns how many opcodes are valid for given sample" $ 
          Day16.solution1 [((3, 2, 1, 1),(9, 2, 1, 2),(3, 2, 2, 1))] `shouldBe` 1
      describe "day 17" $ do
        it "returns how many tiles are wet" $ do
          file <- readFile "./test/day17-example-1.txt"
          let input = Day17.parseInput $ lines file
          Day17.solution1 input `shouldBe` 57
          Day17.solution2 input `shouldBe` 29
        it "returns how many water tiles are left after the water spring stops producing water" $ do
          file <- readFile "./test/day17-example-1.txt"
          let input = Day17.parseInput $ lines file
          Day17.solution2 input `shouldBe` 29
      describe "day 18" $ do
        let scan = [".#.#...|#.",
                    ".....#|##|",
                    ".|..|...#.",
                    "..|#.....#",
                    "#.#|||#|#|",
                    "...#.||...",
                    ".|....|...",
                    "||...#|.#|",
                    "|.||||..|.",
                    "...#.|..|."]
        it "returns resource value after ten minutes" $ do
          let input = Day18.buildMap scan
          Day18.solution1 input `shouldBe` 1147
      describe "day 19" $ do
        let c0de = ["#ip 0",
                    "seti 5 0 1",
                    "seti 6 0 2",
                    "addi 0 1 0",
                    "addr 1 2 3",
                    "setr 1 0 0",
                    "seti 8 0 4",
                    "seti 9 0 5"]
            code = Day19.parseCode c0de
        it "returns final value of each register" $ do
          Day19.solution1 code 0 `shouldBe` 6
          Day19.solution1 code 1 `shouldBe` 5
          Day19.solution1 code 2 `shouldBe` 6
          Day19.solution1 code 3 `shouldBe` 0
          Day19.solution1 code 4 `shouldBe` 0
          Day19.solution1 code 5 `shouldBe` 9
        it "returns value or register 0" $ do
          file <- readFile "./test/day19.txt"
          let input = Day19.parseCode $ lines file
          Day19.solution1 input 0 `shouldBe` 1860
      describe "day 20" $ do
        let r1 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
            r2 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
            r3 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
            m1 = ["###########",
                  "#.|.#.|.#.#",
                  "#-###-#-#-#",
                  "#.|.|.#.#.#",
                  "#-#####-#-#",
                  "#.#.#.|.#.#",
                  "#-#-#####-#",
                  "#.#.|.|.|.#",
                  "#-###-###-#",
                  "#.|.|.#.|.#",
                  "###########"]
            m2 = ["#############",
                  "#.|.|.|.|.|.#",
                  "#-#####-###-#",
                  "#.#.|.#.#.#.#",
                  "#-#-###-#-#-#",
                  "#.#.#.|.#.|.#",
                  "#-#-#-#####-#",
                  "#.#.#.#.|.#.#",
                  "#-#-#-###-#-#",
                  "#.|.#.|.#.#.#",
                  "###-#-###-#-#",
                  "#.|.#.|.|.#.#",
                  "#############"]
            m3 = ["###############",
                  "#.|.|.|.#.|.|.#",
                  "#-###-###-#-#-#",
                  "#.|.#.|.|.#.#.#",
                  "#-#########-#-#",
                  "#.#.|.|.|.|.#.#",
                  "#-#-#########-#",
                  "#.#.#.|.#.|.#.#",
                  "###-#-###-#-#-#",
                  "#.|.#.#.|.#.|.#",
                  "#-###-#####-###",
                  "#.|.#.|.|.#.#.#",
                  "#-#-#####-#-#-#",
                  "#.#.|.|.|.#.|.#",
                  "###############"]
        it "builds correct map from regex expression" $ do
          lines (Day20.printMap (Day20.buildMap r1)) `shouldBe` m1
          lines (Day20.printMap (Day20.buildMap r2)) `shouldBe` m2
          lines (Day20.printMap (Day20.buildMap r3)) `shouldBe` m3
        it "finds distance to the furthest room" $ do
          Day20.solution1 (Day20.buildMap r1) `shouldBe` 18
          Day20.solution1 (Day20.buildMap r2) `shouldBe` 23
          Day20.solution1 (Day20.buildMap r3) `shouldBe` 31
      describe "day 22" $ do
        it "finds total risk level of specified area" $ 
          Day22.solution1 (Day22.Point 10 10) 510 `shouldBe` 114
        it "shortest distance to the target" $ 
          Day22.solution2 (Day22.Point 10 10) 510 `shouldBe` 45
      describe "day 23" $ 
        it "finds the coordinate in range of the most nanobots" $ do
          let bots = [Day23.Nanobot (Day23.Point 10 12 12) 2,
                      Day23.Nanobot (Day23.Point 12 14 12) 2,
                      Day23.Nanobot (Day23.Point 16 12 12) 4,
                      Day23.Nanobot (Day23.Point 14 14 14) 6,
                      Day23.Nanobot (Day23.Point 50 50 50) 200,
                      Day23.Nanobot (Day23.Point 10 10 10) 5]
          Day23.solution2 bots `shouldBe` 36
      describe "day 24" $ do
        let group1 = Group "m1" 17 5390 (Set.fromList [Radiation, Bludgeoning]) Set.empty Fire TeamA 4507 2
            group2 = Group "m2" 989 1274 (Set.fromList [Bludgeoning, Slashing]) (Set.fromList [Fire]) Slashing TeamA 25 3
            group3 = Group "n1" 801 4706 (Set.fromList [Radiation]) Set.empty Bludgeoning TeamB 116 1
            group4 = Group "n2" 4485 2961 (Set.fromList [Fire, Cold]) (Set.fromList [Radiation]) Slashing TeamB 12 4
        it "returns the sum of survived units" $ 
          Day24.solution1 [group1, group2, group3, group4] `shouldBe` 5216
        it "determines whether team can win after a boost" $ do
          Day24.boostedTeamWins 0 TeamA [group1, group2, group3, group4] `shouldBe` (False, 5216)
          Day24.boostedTeamWins 1570 TeamA [group1, group2, group3, group4] `shouldBe` (True, 51)
      describe "day 25" $ do
        let example0 = [(0,0,0,0),(3,0,0,0),(0,3,0,0),(0,0,3,0),(0,0,0,3),(0,0,0,6),(9,0,0,0),(12,0,0,0)]
            example1 = [(-1,2,2,0),(0,0,2,-2),(0,0,0,-2),(-1,2,0,0),(-2,-2,-2,2),(3,0,2,-1),(-1,3,2,2),(-1,0,-1,0),(0,2,1,-2),(3,0,0,0)]
            example2 = [(1,-1,0,1),(2,0,-1,0),(3,2,-1,0),(0,0,3,1),(0,0,-1,-1),(2,3,-2,0),(-2,2,0,0),(2,-2,0,-1),(1,-1,0,-1),(3,2,0,2)]
            example3 = [(1,-1,-1,-2),(-2,-2,0,1),(0,2,1,3),(-2,3,-2,1),(0,2,3,-2),(-1,-1,1,-2),(0,-2,-1,0),(-2,2,3,-1),(1,2,2,0),(-1,-2,0,-2)]
        it "returns number of constellations" $ do         
          Day25.solution1 example0 `shouldBe` 2
          Day25.solution1 example1 `shouldBe` 4
          Day25.solution1 example2 `shouldBe` 3
          Day25.solution1 example3 `shouldBe` 8

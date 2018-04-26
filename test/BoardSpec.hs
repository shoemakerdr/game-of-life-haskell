module BoardSpec (main, spec) where

import Board (Board, Cell(..), nextGeneration, progressCell, cycleCell, limiter, countNeighbors)
import Data.Matrix (fromLists, nrows, ncols)
import Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "nextGeneration" $
        it "progresses all cells on the board to the next generation" $ do
            let board1 = fromLists [ [ Alive, Dead, Dead ]
                                   , [ Dead, Dead, Dead ]
                                   , [ Dead, Alive, Dead ]
                                   ]
                board2 = fromLists [ [ Alive, Dead, Dead ]
                                   , [ Alive, Alive, Alive ]
                                   , [ Dead, Alive, Dead ]
                                   ]
                board2Next = fromLists [ [ Alive, Dead, Dead ]
                                       , [ Alive, Dead, Alive ]
                                       , [ Alive, Alive, Alive ]
                                       ]
                fullBoard = fromLists [ [ Alive, Alive, Alive ]
                                      , [ Alive, Alive, Alive ]
                                      , [ Alive, Alive, Alive ]
                                      ]
                fullBoardNext = fromLists [ [ Alive, Dead, Alive ]
                                          , [ Dead, Dead, Dead ]
                                          , [ Alive, Dead, Alive ]
                                          ]
            nextGeneration board1 `shouldBe` deadBoard
            nextGeneration board2 `shouldBe` board2Next
            nextGeneration fullBoard `shouldBe` fullBoardNext

    describe "progressCell" $
        it "gets next lifecycle of cell" $ do
            let board = fromLists [ [ Alive, Dead, Dead ]
                                  , [ Alive, Alive, Alive ]
                                  , [ Dead, Alive, Dead ]
                                  ]
            progressCell board (1, 1) `shouldBe` Alive
            progressCell board (1, 2) `shouldBe` Dead
            progressCell board (1, 3) `shouldBe` Dead
            progressCell board (2, 1) `shouldBe` Alive
            progressCell board (2, 2) `shouldBe` Dead
            progressCell board (2, 3) `shouldBe` Alive
            progressCell board (3, 1) `shouldBe` Alive
            progressCell board (3, 2) `shouldBe` Alive
            progressCell board (3, 3) `shouldBe` Alive

    describe "cycleCell" $ do
        describe "with an alive cell" $ do
            it "will die with fewer than two neighbors" $
                cycleCell 1 Alive `shouldBe` Dead
            it "will stay alive with two or three neighbors" $ do
                cycleCell 2 Alive `shouldBe` Alive
                cycleCell 3 Alive `shouldBe` Alive
            it "will die with more than three neighbors" $ do
                cycleCell 4 Alive `shouldBe` Dead
                cycleCell 5 Alive `shouldBe` Dead
                cycleCell 6 Alive `shouldBe` Dead
                cycleCell 7 Alive `shouldBe` Dead
                cycleCell 8 Alive `shouldBe` Dead
        describe "with a dead cell" $ do
            it "will become alive if it has three neighbors" $
                cycleCell 3 Dead `shouldBe` Alive
            it "stay dead if less than three neighbors" $ do
                cycleCell 2 Dead `shouldBe` Dead
                cycleCell 1 Dead `shouldBe` Dead
                cycleCell 0 Dead `shouldBe` Dead
            it "will die with more than three neighbors" $ do
                cycleCell 4 Dead `shouldBe` Dead
                cycleCell 5 Dead `shouldBe` Dead
                cycleCell 6 Dead `shouldBe` Dead
                cycleCell 7 Dead `shouldBe` Dead
                cycleCell 8 Dead `shouldBe` Dead
    describe "limiter" $
        it "limits a given int so it is within the bounds of a given matrix" $ do
            limiter deadBoard nrows 0 `shouldBe` 1
            limiter deadBoard nrows 4 `shouldBe` 3
            limiter deadBoard nrows 2 `shouldBe` 2

    describe "countNeighbors" $
        it "counts the number of living neighbors" $ do
            let board = fromLists [ [ Alive, Dead, Dead ]
                                  , [ Alive, Alive, Alive ]
                                  , [ Dead, Alive, Dead ]
                                  ]
            countNeighbors (1,1) board `shouldBe` 2
            countNeighbors (1,2) board `shouldBe` 4
            countNeighbors (1,3) board `shouldBe` 2
            countNeighbors (2,1) board `shouldBe` 3
            countNeighbors (2,2) board `shouldBe` 4
            countNeighbors (2,3) board `shouldBe` 2
            countNeighbors (3,1) board `shouldBe` 3
            countNeighbors (3,2) board `shouldBe` 3
            countNeighbors (3,3) board `shouldBe` 3

deadBoard = fromLists [ [ Dead, Dead, Dead ]
                      , [ Dead, Dead, Dead ]
                      , [ Dead, Dead, Dead ]
                      ]


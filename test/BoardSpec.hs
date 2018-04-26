module BoardSpec (main, spec) where



import Board (Board, Cell(..), nextGeneration, progressCell, cycleCell)
import Data.Matrix (fromLists)
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "nextGeneration" $
        it "kills cells with less than two neighbors" $ do
            let board = fromLists [ [ Alive, Dead, Dead ]
                                  , [ Dead, Dead, Dead ]
                                  , [ Dead, Alive, Dead ]
                                  ]
            nextGeneration board `shouldBe` deadBoard

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
        describe "an alive cell" $ do
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
        describe "a dead cell" $ do
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

deadBoard = fromLists [ [ Dead, Dead, Dead ]
                      , [ Dead, Dead, Dead ]
                      , [ Dead, Dead, Dead ]
                      ]



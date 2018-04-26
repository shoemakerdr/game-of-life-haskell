module Board where



import Data.Matrix ( Matrix, getElem, submatrix
                   , toList, toLists, nrows, ncols
                   , fromList
                   )
import Debug.Trace (traceShow, traceShowId)



data Cell
    = Dead
    | Alive
        deriving (Show, Eq)


type Board =
    Matrix Cell


type Position = (Int, Int)



nextGeneration :: Board -> Board
nextGeneration board =
    traceShow something $ fmap (const Dead) board
    -- fromList rows cols
        -- (progressCell board <$> zip [1..rows] [1..cols])
    where
        rows = nrows board
        cols = ncols board
        something = progressCell board <$> [(x,y) | x <- [1..rows], y <- [1..cols]]


progressCell :: Board -> Position -> Cell
progressCell board pos =
    cycleCell (neighbors pos board) cell
        where
            cell = getElem row col board
            row = fst pos
            col = snd pos
            neighbors pos board =
                (+) offset $ length $ filter (Alive ==) neighborhood
                    where
                        neighborhood =
                            toList $ 
                                submatrix 
                                    (limit $ row - 1)
                                    (limit $ row + 1)
                                    (limit $ col - 1)
                                    (limit $ col + 1) board
                        offset =
                            case cell of
                                Alive -> -1
                                Dead -> 0
                        limit = limitInt board
                        limitInt matrix i 
                            | i < 1 = 1
                            | i > upper = upper
                            | otherwise = i
                                where
                                    upper = length $ head $ toLists matrix

cycleCell :: Int -> Cell -> Cell
cycleCell neighbors Alive
    | neighbors < 2 = Dead
    | neighbors > 3 = Dead
    | otherwise = Alive
cycleCell neighbors Dead
    | neighbors == 3 = Alive
    | otherwise = Dead

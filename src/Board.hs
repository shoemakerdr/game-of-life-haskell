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
    -- traceShow something $ fmap (const Dead) board
    fromList rows cols progressed
    where
        rows = nrows board
        cols = ncols board
        progressed =
            progressCell board
                <$> [(x,y) | x <- [1..rows], y <- [1..cols]]


progressCell :: Board -> Position -> Cell
progressCell board pos =
    cycleCell (countNeighbors pos board) cell
        where
            cell = uncurry getElem pos board


cycleCell :: Int -> Cell -> Cell
cycleCell neighbors Alive
    | neighbors < 2 = Dead
    | neighbors > 3 = Dead
    | otherwise = Alive
cycleCell neighbors Dead
    | neighbors == 3 = Alive
    | otherwise = Dead


limiter :: Matrix a -> (Matrix a -> Int) -> Int -> Int
limiter matrix withinUpper i
    | i < 1 = 1
    | i > upper = upper
    | otherwise = i
        where
            upper = withinUpper matrix


countNeighbors :: Position -> Board -> Int
countNeighbors pos board =
    (+) offset $ length $ filter (Alive ==) neighborhood
        where
            row = fst pos
            col = snd pos
            neighborhood =
                toList $ 
                    submatrix 
                        (limitRow $ row - 1)
                        (limitRow $ row + 1)
                        (limitCol $ col - 1)
                        (limitCol $ col + 1) board
            cell = getElem row col board
            offset =
                case cell of
                    Alive -> -1
                    Dead -> 0
            limitRow = limiter board nrows
            limitCol = limiter board ncols



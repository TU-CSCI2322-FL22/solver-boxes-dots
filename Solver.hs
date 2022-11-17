module Solver
( whoWillWin
, bestMove
) where

import GameState
import Data.Maybe

whoWillWin :: Board -> Win
whoWillWin board@(_,_,_,_,player) = case checkWin board of
    Just (Winner x) -> Winner x
    Just Tie -> Tie
    Nothing
        | (Winner player) `elem` it -> Winner player
        | Tie `elem` it -> Tie
        | otherwise -> Winner (negPlayer player)
        where it =  map whoWillWin $ catMaybes $ map (updateBoard board) (validMoves board)

bestMove :: Board -> Move
bestMove board@(_,_,_,_,player) = undefined
-- "He ain't beating Goku tho" - Matt
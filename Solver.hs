module Solver
( whoWillWin
, bestMove
) where

import GameState
import Data.Maybe
import Data.List

whoWillWin :: Board -> Win
whoWillWin board@(_,_,_,_,player) = case checkWin board of
    Just (Winner x) -> Winner x
    Just Tie -> Tie
    Nothing
        | (Winner player) `elem` vegeta -> Winner player
        | Tie `elem` vegeta -> Tie
        | otherwise -> Winner (negPlayer player)
        where vegeta =  map whoWillWin $ catMaybes $ map (updateBoard board) (validMoves board)

bestMove :: Board -> Move
bestMove board@(_,_,_,_,player) 
    | winnerPlayer /= Nothing = fromJust winnerPlayer
    | tier /= Nothing = fromJust tier
    | otherwise = ku
    where winnerPlayer = (Winner player) `lookup` (gohan)
          tier = Tie `lookup` gohan
          gohan@((go,ku):chan) = catMaybes $ map krillin (validMoves board)
          krillin :: Move -> Maybe (Win,Move)
          krillin move = case updateBoard board move of
            Nothing -> Nothing
            Just x -> Just (whoWillWin x, move)

-- "He ain't beating Goku tho" - Matt
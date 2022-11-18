module Solver
( whoWillWin
, bestMove
) where

import GameState
import Data.Maybe
import Data.List


-- checks who will win/tie from the given board state
whoWillWin :: Board -> Win
whoWillWin board@(_,_,_,_,player) =
    case checkWin board of
         Just (Winner x) -> Winner x
         Just Tie -> Tie
         Nothing
            | (Winner player) `elem` vegeta -> Winner player
            | Tie `elem` vegeta -> Tie
            | otherwise -> Winner (negPlayer player)
            where vegeta =  map whoWillWin $ catMaybes $ map (updateBoard board) (validMoves board)

-- gives the best move for the player to make based on which move will force a win/tie
bestMove :: Board -> Maybe Move
bestMove (_,_,[],_,_) = Nothing
bestMove board@(_,_,_,_,player) =
    case ((Winner player) `lookup` goku, Tie `lookup` goku, goku) of
        (Just wm, _, _) -> Just wm
        (Nothing, Just tm, _) -> Just tm
        (Nothing, Nothing, (wn,mv):_) -> Just mv
    where goku = catMaybes $ map krillin (validMoves board)
          krillin :: Move -> Maybe (Win,Move)
          krillin move = case updateBoard board move of
                            Nothing -> Nothing
                            Just x -> Just (whoWillWin x, move)

-- "He ain't beating Goku tho" - Matt
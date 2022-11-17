import GameState
import Data.Maybe

whoWillWin :: Board -> Win
whoWillWin board = case checkWin board of
    Just (Winner x) -> Winner x
    Just Tie -> Tie
    Nothing -> map whoWillWin $ catMaybes [(updateBoard board move) | move <- (validMoves board)]

bestMove :: Board -> Move
bestMove = undefined
-- "He ain't beating Goku tho" - Matt
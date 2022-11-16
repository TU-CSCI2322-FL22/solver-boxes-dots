import GameState

whoWillWin :: Board -> Win
whoWillWin board =
    whoWillWinHelper board move = case checkWin board of
        Winner x -> Winner x
        Tie -> Tie
        Nothing -> whoWillWinHelper (updateBoard)

bestMove :: Board -> Move
bestMove = undefined

-- "He ain't beating Goku tho" - Matt
module Solver
( whoWillWin
, bestMove
, putGame
, readGame
, writeGame
, loadGame
, putWinner
) where

import GameState
import Data.Maybe
import Data.List
import Debug.Trace

-------------------------------------------------------------------------------------------------
--                                  SOLVING THE GAME
-------------------------------------------------------------------------------------------------

-- checks who will win/tie from the given board state
whoWillWin :: Board -> Win
whoWillWin board@(_,_,legals,_,player) = 
    case checkWin board of
         Just x -> x
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

-- evaluates the board based on the Red player
evaluate :: Board -> Int
evaluate board@(_,boxes,_,_,_) = redBoxes - blueBoxes
    where redBoxes = foldr (\(_,player) acc -> if player == Red then 1 + acc else acc) 0 boxes
          blueBoxes = abs $ length boxes - redBoxes

-------------------------------------------------------------------------------------------------
--                           READING/WRITING/PRINTING GAMESTATE
-------------------------------------------------------------------------------------------------

putGame :: Board -> IO ()
putGame board = putStr $ prettyShowBoard board

readGame :: String -> Board
readGame = read

writeGame :: Board -> FilePath -> IO ()
writeGame board file = do
    writeFile file (show board)
    return ()

loadGame :: FilePath -> IO Board 
loadGame file = do
    contents <- readFile file
    return $ read contents

putWinner :: Board -> IO ()
putWinner board = putStr $ "Result: " ++ (show $ whoWillWin board)  ++ "\n"

-- "He ain't beating Goku tho" - Matt
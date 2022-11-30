module Solver
( whoWillWin
, bestMove
, putGame
, readGame
, writeGame
, loadGame
, putWinner
, whoMightWin
, aMove
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
            where vegeta =  map whoWillWin $ mapMaybe (updateBoard board) (validMoves board)

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
evaluate board@(boxes,_,_,size,_) = case checkWin board of
    Just (Winner Red) -> size^2 + 1
    Just (Winner Blue) -> - (size^2 + 1)
    Just Tie -> 0
    Nothing -> redBoxes - blueBoxes
        where redBoxes = foldr (\(_,player) acc -> if player == Red then 1 + acc else acc) 0 boxes
              blueBoxes = abs $ length boxes - redBoxes

-- goes to a constanct depth to figure out who might win given a board
whoMightWin :: Int -> Board -> Win
whoMightWin _ board@(_,_,[],_,_)
    | score > 0 = Winner Red
    | score < 0 = Winner Blue
    | otherwise = Tie
    where score = evaluate board
whoMightWin 0 board
    | score > 0 = Winner Red
    | score < 0 = Winner Blue
    | otherwise = Tie
    where score = evaluate board
whoMightWin depth board@(_,_,legals,_,player) 
    | (Winner player) `elem` vegeta = Winner player
    | Tie `elem` vegeta = Tie
    | otherwise = Winner (negPlayer player)
        where vegeta =  map (whoMightWin (depth-1)) (mapMaybe (updateBoard board) (validMoves board))

-- gets the best move that can be made by going only a constant depth
aMove :: Board -> Int -> Maybe Move
aMove (_,_,[],_,_) _ = Nothing
aMove board@(_,_,_,_,player) depth =
    case ((Winner player) `lookup` goku, Tie `lookup` goku, goku) of
        (Just wm, _, _) -> Just wm
        (Nothing, Just tm, _) -> Just tm
        (Nothing, Nothing, (wn,mv):_) -> Just mv
    where goku = catMaybes $ map krillin (validMoves board)
          krillin :: Move -> Maybe (Win,Move)
          krillin move = case updateBoard board move of
                            Nothing -> Nothing
                            Just x -> Just (whoMightWin depth x, move)

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
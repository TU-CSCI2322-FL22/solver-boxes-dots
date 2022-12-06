module GameState
( Board (..)
, Line (..)
, Box (..)
, Move (..)
, Player (..)
, Win (..)
, Direction (..)
, prettyShowBoard
, makeBoard
, legalMoves
, validMoves
, makeMove
, makeBox
, updateBoard
, checkWin
, putGame
, readGame
, writeGame
, loadGame
, putWinner
) where

import Data.List
-------------------------------------------------------------------------------------------------
--                              DATA TYPES FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------

-- Represents the players of the game devided into the colors red and blue
data Player = Red | Blue deriving (Show,Eq,Read)
-- Describes the winner or a tie
data Win = Winner Player | Tie deriving Show
-- a 2D point to represent the positions on the board 
type Point = (Int,Int) 
-- Represent the direction of the lines
data Direction = Rght | Dwn deriving (Show, Eq, Read)
-- Represents a line made of two points, the given and the one in the direction given
type Line = (Point, Direction)
-- Represents a box with the point in the top left  corner and the player who made it
type Box = (Point,Player)
-- Represents the move of a player
type Move = Line
-- Represents the legal moves that can be made
type LegalMoves = [Line]
-- Represents the game state as 
-- include the boxes that have been made, the lines, what turn it is, and whose turn it is
type Board = ([Box], [Line], LegalMoves, Int, Player)

-------------------------------------------------------------------------------------------------
--                             PRETTY SHOW FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------

prettyShowBoard :: Board -> String
prettyShowBoard board@(boxes, lines, legals, size, player) = case checkWin board of
    Nothing -> ("Current Player: " ++ show player ++ "\n") ++ prettyBoardSwag points []
    Just (Winner player) -> ("Winner: " ++ show player ++ "\n") ++ prettyBoardSwag points []
    Just Tie -> ("TIE\n") ++ prettyBoardSwag points []
    where points = [(x,y) | x <- [1..size], y <- [1..size]]
          prettyBoardSwag [] acc = []
          prettyBoardSwag (p:ps) acc
            | (p,Rght) `elem` lines && (p,Dwn) `elem` lines = show p ++ "--" ++ prettyBoardSwag ps (acc ++ ("  |  " ++ plyr))
            | (p,Rght) `elem` lines = show p ++ "--" ++ prettyBoardSwag ps (acc ++ "       ")
            | (p,Dwn) `elem` lines = if snd p /= size then show p ++ "  " ++ prettyBoardSwag ps (acc ++ ("  |    ")) else show p ++ "\n" ++ acc ++ "  |\n" ++ prettyBoardSwag ps []
            | otherwise = if snd p /= size then show p ++ "  " ++ prettyBoardSwag ps (acc ++ "       ") else show p ++ "\n" ++ acc ++ "\n" ++ prettyBoardSwag ps []
                where plyr = if (p,Blue) `elem` boxes then "Bl" else if (p,Red) `elem` boxes then "Rd" else "  "
-- (1,1)--(1,2)--(1,3)
--   |  Rd  |      |
-- (2,1)--(2,2)--(2,3)
--   |  Rd  |            
-- (3,1)--(3,2)  (3,3)

-------------------------------------------------------------------------------------------------
--                             FUNCTIONS FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------

-- constructs the starting board by taking an int which represents the sqrt of the number of dots
-- inputing 3 would make a 3x3 board
makeBoard :: Int -> Maybe Board
makeBoard size 
    | size > 2 = Just ([], [], legalMoves size, size, Red)
    | otherwise = Nothing

-- creates a list of all the legal moves that can be made with the given board
legalMoves :: Int -> [Line]
legalMoves size = legalMovesHelper 1 1 
    where legalMovesHelper row col
            | row == size && col == size = [] 
            | col == size                = ((row, col), Dwn) : (legalMovesHelper (row + 1) 1)
            | row == size                = ((row, col), Rght) : (legalMovesHelper row (col + 1))
            | otherwise                  = ((row,col), Dwn) : ((row,col), Rght) : (legalMovesHelper row (col + 1))

validMoves :: Board -> [Line]
validMoves (_, _, legalMoves, _, _) = legalMoves

-- if the move is legal, it returns a line that can be played, else, it returns nothing
makeMove :: Point -> Point -> Board -> Maybe Move
makeMove (row1,col1) (row2,col2) (_, lines, legals, _, player)
    | (abs (row1-row2) == 1 && col1 == col2) = makeMoveHelper (((min row1 row2), col1), Dwn)
    | (abs (col1-col2) == 1 && row1 == row2) = makeMoveHelper ((row1, (min col1 col2)), Rght)
    | otherwise = Nothing
        where makeMoveHelper line 
                | line `elem` legals = Just line
                | otherwise = Nothing

-- makes a box out of the given lines and the player that made it
makeBox :: Move -> Board -> [Box]
makeBox line@((row, col), direction) (boxes, lines, _, _, player) =
    if direction == Rght 
    then checkBoxHelper(row-1, col)++checkBoxHelper(row, col)
    else checkBoxHelper(row, col-1)++checkBoxHelper(row, col)
        where checkBoxHelper point@(row2, col2) = 
                if l `intersect` (line:lines) ==  l
                then [(point, player)]
                else []
                    where l = [(point, Rght),(point, Dwn), ((row2+1, col2), Rght), ((row2, col2+1), Dwn)]

-- takes in a line and adds it to the board
-- checks if the line can form a new box using "canMakeBox"
-- if it can, it makes a box and adds it to the list as well
-- it changes the player if a box wasn't made and keeps the player the same if not
updateBoard :: Board -> Move -> Maybe Board
updateBoard board@(boxes, lines, legals, size, player) line
    | line `elem` legals = case makeBox line board of
                            [] -> Just (boxes, line:lines, (delete line legals), size, (negPlayer player))
                            x -> Just (x++boxes, line:lines, (delete line legals), size, player)
    | otherwise = Nothing
        where negPlayer Red = Blue
              negPlayer Blue = Red

-- Checks the board if someone has won, tied, or if the game is still going by returning nothing
checkWin :: Board -> Maybe Win
checkWin (boxes, _, _, size, player) 
    | isDone && redBoxes < halfBoxes = Just (Winner Blue)
    | isDone && redBoxes > halfBoxes = Just (Winner Red)
    | isDone                         = Just (Tie)
    | otherwise                      = Nothing
        where halfBoxes = numBoxes `div` 2 
              isDone = length boxes == numBoxes
              numBoxes = (size - 1)^2
              redBoxes = foldr (\(_,player) acc -> if player == Red then 1 + acc else acc) 0 boxes


-------------------------------------------------------------------------------------------------
                           READING/WRITING/PRINTING GAMESTATE
-------------------------------------------------------------------------------------------------
{-
something like this for showGame maybe, I can fix variable names and efficiency later, still doesn't compile
bxsLns subj = foldr (\h z -> h ++ z) "" [(show x) ++ "," ++ (show y) ++ ")," ++ snd(filter (\t -> pd == fst t) [(Red, "Red"), (Blue, "Blue"), (Rght, "Rght"), (Dwn, "Dwn")]) ++ "/&/" | ((x, y), pd)]

showGame :: board -> String
showGame board =
    let (boxes, lines, legals, sz, player) = board
    in (bxsLns boxes) ++ "/#/" ++ (bxsLns lines) ++ "/#/" ++ (bxsLns legals) ++ "/#/" ++ (show sz) ++ "/#/" ++ (if player == Red then "Red" else "Blue")
-}


putGame :: Board -> IO ()
putGame board = putStr $ prettyShowBoard board
foldr (\h z -> h ++ z) "" [(show x) ++ "," ++ (show y) ++ ")," ++ snd(filter (\t -> pd == fst t) [(Red, "Red"), (Blue, "Blue"), (Rght, "Rght"), (Dwn, "Dwn")]) ++ "/&/" | ((x, y), pd)]

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
putWinner board = case checkWin board of
    Just (Winner x) -> do
        putStr ("Winner is: " ++ show x)
        return ()
    Just Tie -> do
        putStr "There is a Tie"
        return ()
    Nothing -> do
        putStr "Game isn't finished yet"
        return ()

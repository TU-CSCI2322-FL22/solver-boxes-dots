import Data.List
-------------------------------------------------------------------------------------------------
--                              DATA TYPES FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------

-- Represents the players of the game devided into the colors red and blue
data Player = Red | Blue deriving (Show,Eq)
-- Describes the winner or a tie
data Win = Winner Player | Tie deriving Show
-- a 2D point to represent the positions on the board 
type Point = (Int,Int) 
-- Represent the direction of the lines
data Direction = Rght | Dwn deriving (Show, Eq)
-- Represents a line made of two points, the given and the one in the direction given
type Line = (Point, Direction)
-- Represents a box with the point in the top left  corner and the player who made it
type Box = (Point,Player)
-- Represents the move of a player
type Move = (Line,Player)
-- Represents the legal moves that can be made
type LegalMoves = [Line]
-- Represents the game state as 
-- include the boxes that have been made, the lines, what turn it is, and whose turn it is
type Board = ([Box], [Line], LegalMoves, Int, Player)

-------------------------------------------------------------------------------------------------
--                             PRETTY SHOW FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------
prettyShowBoard :: Board -> String
prettyShowBoard (boxes,lines, legals, _, player) = undefined

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

-- if the move is legal, it returns a line that can be played, else, it returns nothing
makeMove :: Point -> Point -> Board -> Maybe Move
makeMove (row1,col1) (row2,col2) (_, lines, legals, _, player)
    | (abs (row1-row2) == 1 && col1 == col2) = makeMoveHelper (((min row1 row2), col1), Dwn)
    | (abs (col1-col2) == 1 && row1 == row2) = makeMoveHelper ((row1, (min col1 col2)), Rght)
    | otherwise = Nothing
        where makeMoveHelper line 
                | line `elem` legals = Just (line, player) 
                | otherwise = Nothing

-- makes a box out of the given lines and the player that made it
makeBox :: Move -> Board -> [Box]
makeBox (line@((row, col), direction), player) (boxes, lines, _, _, _) =
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
updateBoard :: Board -> Move -> Board
updateBoard board@(boxes, lines, legals, size, currentP) move@(line, player) = case makeBox move board of
    [] -> (boxes, line:lines, (delete line legals), size, (negPlayer player))
    x -> (x++boxes, line:lines, (delete line legals), size, player)
    where negPlayer Red = Blue
          negPlayer Blue = Red



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

Just gameState = makeBoard 3
Just move = makeMove (1,1) (1,2) gameState

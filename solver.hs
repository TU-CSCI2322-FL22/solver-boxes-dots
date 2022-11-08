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
data Direction = Down | Left deriving (Show, Eq)
-- Represents a line made of two points, the given and the one in the direction given
type Line = (Point, Direction)
-- Represents a box with the point in the top left corner and the player who made it
type Box = (Point,Player)
-- Represents the move of a player
type Move = (Line,Player)
-- Represents the game state as 
-- include the boxes that have been made, the lines, what turn it is, and whose turn it is
type Board = ([Box],[Line],Int,Player)
-------------------------------------------------------------------------------------------------
--                             PRETTY SHOW FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------
prettyShowBoard :: Board -> String
prettyShowBoard = undefined
-------------------------------------------------------------------------------------------------
--                             FUNCTIONS FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------
-- constructs the starting board by taking an int which represents the sqrt of the number of dots
-- inputing 3 would make a 3x3 board
makeBoard :: Int -> Board
makeBoard = undefined
-- checks if the move is legal, aka all points are on the board, checks if the points are one apart
-- in one of the allowed directions, and lastly checks if the line is already on the board
legalMoves :: Board -> [Line]
legalMoves = undefined
-- if the move is legal, it returns a line that can be played, else, it returns nothing
makeMove :: Point -> Point -> Maybe Move
makeMove = undefined
-- takes in a line and adds it to the board
-- checks if the line can form a new box using "canMakeBox"
-- if it can, it makes a box and adds it to the list as well
-- it changes the player if a box wasn't made and keeps the player the same if not
updateBoard :: Board -> Move -> Maybe Board
updateBoard = undefined
-- checks if someone has won the game or tied and if neither has happened then it will return nothing
-- meaning the game must still go on
checkWin :: Board -> Maybe Win
checkWin = undefined
-- makes a box out of the given lines and the player that made it
makeBox :: Move -> Board -> Maybe Box
makeBox = undefined

gameState = makeBoard 3
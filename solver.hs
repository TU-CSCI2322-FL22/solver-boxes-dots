-------------------------------------------------------------------------------------------------
--                              DATA TYPES FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------
-- Represents the players of the game devided into the colors red and blue
data Player = Red | Blue
-- a 2D point to represent the positions on the board 
type Point = (Int,Int) 
-- Represent the direction of the lines
data Direction = Down | Left
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
instance Show Board where
    show board = undefined

instance Show Line where
    show line = undefined

instance Show Box where
    show box = undefined

instance Show Player where
    show Red = "red"
    show Blue = "blue"
-------------------------------------------------------------------------------------------------
--                             FUNCTIONS FOR DOTS AND BOXES
-------------------------------------------------------------------------------------------------
makeBoard :: Int -> Board
makeBoard = undefined

makeMove :: Point -> Point -> Board -> Maybe Line
makeMove = undefined

isMoveLegal :: Point -> Point -> Board -> Boolean
isMoveLegal = undefined

updateBoard :: Board -> Line -> Board
updateBoard = undefined

winnerCheck :: Board -> Player
winnerCheck = undefined

canMakeBox :: [Line] -> Boolean
canMakeBox = undefined

makeBox :: [Line] -> Player -> Box
makeBox = undefined



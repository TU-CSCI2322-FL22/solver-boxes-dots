import GameState
import Solver

Just startBoard3 = makeBoard 3
Just startBoard4 = makeBoard 4
startPrint = putGame startBoard3
boardWithBox = ([((1,1), Blue)],[((1,1),Dwn),((1,1),Rght),((1,2),Dwn),((2,1),Rght)],[((1,2),Rght), ((1,3),Dwn),((2,1),Dwn),((2,2),Dwn),((2,2),Rght),((2,3),Dwn),((3,1),Rght),((3,2),Rght)],3,Blue)

wholeBoard3 = ([((1,1),Blue),((1,2),Blue),((2,1),Blue),((2,2),Red)], (legalMoves 3), [], 3, Blue)
wholeBoard4 = ([((1,1),Blue),((1,2),Blue),((2,1),Blue),((2,2),Red),((1,3),Red),((2,3),Blue),((3,1),Blue),((3,2),Red),((3,3),Blue)], (legalMoves 4), [], 4, Blue)
Just testBoard = updateBoard boardWithBox ((1,3),Dwn)
Just testBoard2 = updateBoard testBoard ((3,1),Rght)



printWholeBoard3 = putGame wholeBoard3
printTest = putGame testBoard
it = read "([((1,1), Blue)],[((1,1),Dwn),((1,1),Rght),((1,2),Dwn),((2,1),Rght)],[((1,2),Rght), ((1,3),Dwn),((2,1),Dwn),((2,2),Dwn),((2,2),Rght),((2,3),Dwn),((3,1),Rght),((3,2),Rght)],3,Blue)" :: Board

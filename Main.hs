module Main where
import GameState
import Solver
import System.IO
import System.Environment

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

main :: IO ()
main = do
  args <- getArgs
  let fname = if null args then "gamestate.txt" else head args
  game <- loadGame fname
  putStr "Move Format: (x1,y1) (x2,y2)"
  let move = getMove game
  return ()

getMove :: Board -> Move
getMove board = do
    input <- prompt "Input move"
    let (p1:p2:ps) = map read (words input) :: [Point]
    let move = makeMove p1 p2 board
    case move of
        Nothing -> getMove board
        Just x -> x
        


prompt :: String -> IO String
prompt question = do
  putStr $ question ++ ": "
  hFlush stdout
  response <- getLine
  return response
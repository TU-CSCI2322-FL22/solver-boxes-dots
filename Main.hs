module Main where
import GameState
import Solver
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fname =  "gamestate.txt"
  game <- loadGame fname
  putWinner game
  return ()
  

--   putStr "Move Format: (x1,y1) (x2,y2)"
--   let move = getMove game
--   return ()

-- getMove :: Board -> Move
-- getMove board = do
--     input <- prompt "Input move"
--     let (p1:p2:ps) = map read (words input) :: [Point]
--     let move = makeMove p1 p2 board
--     case move of
--         Nothing -> getMove board
--         Just x -> x
        


-- prompt :: String -> IO String
-- prompt question = do
--   putStr $ question ++ ": "
--   hFlush stdout
--   response <- getLine
--   return response
module Main where
import GameState
import Solver
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fname = if null args then "TestFiles/gamestate.txt" else head args
  game <- loadGame fname
  putWinner game
  return ()
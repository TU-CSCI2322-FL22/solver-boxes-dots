module Main where
import GameState
import Solver
import System.IO
import System.Environment
import System.Console.GetOpt


data Flag = Help | Winna | Depth String | Move String | Verbose | Interactive deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winna) "Prints out the best move of the given gamestate." 
          , Option ['d'] ["depth"] (ReqArg Depth "<depth>") "Changes the depth to <depth>."
          , Option ['m'] ["move"] (ReqArg Move "<move>") "Updates the board with the given move."
          , Option ['v'] ["verbose"] (NoArg Verbose) "Outputs both the move and a description of how good it is." 
          ]

main :: IO ()
main = do
  args <- getArgs
  let (flags, inputs, error) = getOpt Permute options args
  if (Help `elem` flags) 
    then putStrLn $ usageInfo "Main [option] [file]" options
    else do
      let fname = if null inputs then "TestFiles/gamestate.txt" else head inputs
      game <- loadGame fname
      winnerCheck flags game
      return ()

-------------------------------------------------------------------------------------------------
--                                  FLAG FUNCTIONS                                             --
-------------------------------------------------------------------------------------------------

winnerCheck :: [Flag] -> Board -> IO ()
winnerCheck flags board
  | Winna `elem` flags = putMove board
  | otherwise = return ()

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

putMove :: Board -> IO ()
putMove board = case bestMove board of
  Just x -> putStr $ "Best Move: " ++ (show x) ++ "\n"
  Nothing -> putStr $ "No move can be made!\n"
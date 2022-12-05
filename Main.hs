module Main where
import GameState
import Solver
import System.IO
import Text.Read
import Data.Maybe
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
      moveCheck flags game
      depth <- depthCheck flags game
      verboseCheck flags game depth
      return ()

-------------------------------------------------------------------------------------------------
--                                  FLAG FUNCTIONS                                             --
-------------------------------------------------------------------------------------------------

winnerCheck :: [Flag] -> Board -> IO ()
winnerCheck flags board
  | Winna `elem` flags = putMove board
  | otherwise = return ()

moveCheck :: [Flag] -> Board -> IO ()
moveCheck ((Move x):_) board = 
  case readMove x board of
    Nothing -> error "That's not a move. Try again." 
    Just n -> case updateBoard board n of
      Nothing -> error "That move is illegal!"
      Just x -> putShitGame x
moveCheck (_:flags) board = moveCheck flags board
moveCheck [] board = return ()
  
depthCheck :: [Flag] -> Board -> IO Int
depthCheck ((Depth x):_) board = 
  case readMaybeInt x of
    Nothing -> error "That's not an Int."
    Just n -> return n
depthCheck (_:flags) board = depthCheck flags board
depthCheck [] board = return $ length (validMoves board)

verboseCheck :: [Flag] -> Board -> Int -> IO ()
verboseCheck flags board depth 
  | Verbose `elem` flags = case aMove board depth of
      Nothing -> error "There are no more moves to be made!"
      Just x -> do
        putStr $ "Move: " ++ show x ++ "\n"
        if(length (validMoves board) == 1) 
          then case whoWillWin board of 
            Winner x -> putStr $ show $ "Result: " ++ show x ++ " will win\n"
            Tie -> putStr "Result: It will be a Tie\n"
          else case updateBoard board x of
            Nothing -> error "something really bad happened!"
            Just x -> putStr $ "Rating: " ++ show (evaluate x) ++ "\n"
  | otherwise = return ()
    where 


-------------------------------------------------------------------------------------------------
--                           READING/WRITING/PRINTING GAMESTATE
-------------------------------------------------------------------------------------------------


readMaybePoint :: String -> Maybe Point
readMaybePoint it = readMaybe it :: Maybe Point

readMaybeInt :: String -> Maybe Int
readMaybeInt it = readMaybe it :: Maybe Int

readMove :: String -> Board -> Maybe Move
readMove move board = case sequence(map readMaybePoint (words move)) of
  Nothing -> Nothing
  Just (p1:p2:[]) -> makeMove p1 p2 board
  Just (_) -> Nothing

putGame :: Board -> IO ()
putGame board = putStr $ prettyShowBoard board

putShitGame :: Board -> IO ()
putShitGame board = putStr $ show board ++ "\n"

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
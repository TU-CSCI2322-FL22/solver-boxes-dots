module Main where
import GameState
import Solver
import System.IO
import Text.Read
import Data.Maybe
import System.Directory
import System.Environment
import System.Console.GetOpt


data Flag = Help | Winna | Depth String | Move String | Verbose | Interactive deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['w'] ["winner"] (NoArg Winna) "Prints out the best move of the given gamestate." 
          , Option ['d'] ["depth"] (ReqArg Depth "<depth>") "Changes the depth to <depth>."
          , Option ['m'] ["move"] (ReqArg Move "<move>") "Updates the board with the given move. Move format: \"(r1,c1) (r2,c2)\"."
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
      board <- loadGame fname
      case board of
        Nothing -> do
          putStrLn "That was an invalid file dummy!"
          return ()
        Just game -> do
          if null error then do
            fullCheck flags game
          else putStr $ "Invalid flags:\n" ++ concat error
      

-------------------------------------------------------------------------------------------------
--                                  FLAG FUNCTIONS                                             --
-------------------------------------------------------------------------------------------------

fullCheck :: [Flag] -> Board -> IO ()
fullCheck [Winna] board = putMove board
fullCheck [Move x] board = movePrint x board
fullCheck flags board = 
  if Winna `elem` flags || (isJust $ moveCheck flags)
  then putStrLn "Incompatible flags." 
  else do
    depth <- depthCheck flags board
    move <- printMFBoard depth board
    case move of
      Nothing -> return ()
      Just x -> if Verbose `elem` flags then verboseCheck board x else return ()

printMFBoard :: Int -> Board -> IO (Maybe Move)
printMFBoard depth board = case aMove board depth of 
  Nothing -> do
    putStrLn "No more moves can be made"
    return Nothing
  Just x -> do 
    putStrLn $ "Best move of depth " ++ show depth ++ ": " ++ show x
    return (Just x)

moveCheck :: [Flag] -> Maybe String
moveCheck ((Move x):_) = Just x
moveCheck (_:flags) = moveCheck flags
moveCheck [] = Nothing

movePrint :: String -> Board -> IO ()
movePrint move board = 
  case readMove move board of
    Nothing -> putStrLn "That's not a move. Try again." 
    Just n -> case updateBoard board n of
      Nothing -> putStrLn "That move is illegal!"
      Just x -> putUglyGame x
  
depthCheck :: [Flag] -> Board -> IO Int
depthCheck ((Depth x):_) board = 
  case readMaybeInt x of
    Nothing -> do
      putStrLn "That's not an invalid depth (defaulted to 6)."
      return 6
    Just n -> return n
depthCheck (_:flags) board = depthCheck flags board
depthCheck [] board = return 6

verboseCheck :: Board -> Move -> IO ()
verboseCheck board move =
  if(length (validMoves board) == 1) 
    then case whoWillWin board of 
      Winner x -> putStrLn $ show $ "Result: " ++ show x ++ " will win"
      Tie -> putStrLn "Result: It will be a Tie"
    else case updateBoard board move of
      Nothing -> putStrLn "something really bad happened!"
      Just x -> putStrLn $ "Rating: " ++ show (evaluate x)

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
putGame board = putStr $ showBoard board

putUglyGame :: Board -> IO ()
putUglyGame board = putStrLn $ show board

readGame :: String -> Board
readGame = read

writeGame :: Board -> FilePath -> IO ()
writeGame board file = do
    writeFile file (show board)
    return ()

loadGame :: FilePath -> IO (Maybe Board) 
loadGame file = do
  b <- doesFileExist file
  if b 
      then do
        contents <- readFile file
        return $ readMaybe contents
      else return Nothing

putWinner :: Board -> IO ()
putWinner board = putStr $ "Result: " ++ (show $ whoWillWin board)  ++ "\n"

putMove :: Board -> IO ()
putMove board = case bestMove board of
  Just x -> putStr $ "Best Move: " ++ (show x) ++ "\n"
  Nothing -> putStr $ "No move can be made!\n"
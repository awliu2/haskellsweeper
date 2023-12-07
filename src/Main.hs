module Main where
import Board
    ( play,
      isGameWon,
      isGameLost,
      setNRandomBombs,
      initBoard,
      Move(..),
      Position,
      Board(nc, nr),
      numMarked,
      numBombs )
import System.IO (stdin, hReady, hSetEcho, hSetBuffering, BufferMode(NoBuffering, LineBuffering), nativeNewline)
import System.Console.ANSI
import System.Directory (doesFileExist, getDirectoryContents, renameFile, removeFile)
import System.Exit
import Data.Aeson
import qualified Data.ByteString.Lazy as B


data Model = Model 
    {
    board :: Board,
    cursor :: Position
    } deriving (Eq)
    
type Action = [Char]

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)
toDifficulty :: Int -> Difficulty
toDifficulty 0 = Easy
toDifficulty 1 = Medium
toDifficulty _ = Hard

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    c <- menuController "Welcome to Minesweeper!" ["New Game", "Load Game", "Manage Save Data", "Help", "Quit"]
    case c of
        0 -> playGame
        1 -> do
            model <- loadGame (Model (initBoard 0 0) (0, 0))
            if model == Model (initBoard 0 0) (0, 0) then do
                main
            else do
                controller model
                return ()
        2 -> do
            manageSaveData
            main
        3 -> do
            clearScreen
            putStrLn helpMsg
            putStrLn "Press any key to return to main menu..."
            getKey
            main
        4 -> do
            exit <- menuController "Are you sure you want to quit?" ["Yes", "No"]
            if exit == 0 then do
                putStrLn "Exiting game..."
                exitSuccess
            else do
                main
        _ -> do
            putStrLn "Invalid action!" -- should never happen
            main
    return ()


playGame :: IO ()
playGame = do
    clearScreen
    difficulty <- menuController "Select a difficulty:" ["Easy", "Medium", "Hard", "Cancel"]
    if difficulty == 3 then do
        putStrLn "Game cancelled!"
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        main
    else do
        model <- initModel (toDifficulty difficulty)
        controller model
        return ()

initModel :: Difficulty -> IO Model
initModel difficulty
    | difficulty == Easy = do
        let nr = 8
            nc = 8
            nb = 10
        board <- setNRandomBombs nb (initBoard nr nc)
        return (Model board (0, 0))
    | difficulty == Medium = do
        let nr = 16
            nc = 16
            nb = 40
        board <- setNRandomBombs nb (initBoard nr nc)
        return (Model board (0, 0))
    | difficulty == Hard = do
        let nr = 16
            nc = 31
            nb = 99
        board <- setNRandomBombs nb (initBoard nr nc)
        return (Model board (0, 0))


controller :: Model -> IO Model
controller model = do
    clearScreen
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    printModel model
    if isGameWon (board model) then do
        putStrLn "You won!"
        return model
    else if isGameLost (board model) then do
        putStrLn "You lost!"
        return model
    else do
        key <- getKey
        if key == "p" || key == "\ESC" then do
            model' <- pauseGame model
            controller model'
        else if key == "q" then do
            exitSuccess
            -- print key
            -- print "updating
        else do 
            let model' = update key model
            controller model'

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

update :: Action -> Model -> Model
update action model
    | action == "q" = model
    | action == "w" || action == "\ESC[A" = moveCursor model (-1) 0
    | action == "s" || action == "\ESC[B" = moveCursor model 1 0
    | action == "a" || action == "\ESC[D" = moveCursor model 0 (-1) 
    | action == "d" || action == "\ESC[C" = moveCursor model 0 1
    | action == "r" || action == " " = playMove (Reveal (cursor model)) model
    | action == "m" || action == "x" = playMove (Mark (cursor model)) model
    | otherwise = model

-----------------------------------------
-- Menu Controller --

-- | menuController is a controller for menus
-- It takes a prompt and a list of options and returns the index of the selected option
menuController :: String -> [String] -> IO Int
menuController prompt options =do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    selection <- menuController' prompt options 0
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    return selection

menuController' :: String -> [String] -> Int -> IO Int
menuController' prompt options selected = do
    clearScreen
    putStrLn prompt
    printOptions options selected
    let numOptions = length options
    key <- getKey
    if key == "w" || key == "\ESC[A" then do
        -- move up
        let selected' = (selected - 1) `mod` numOptions
        menuController' prompt options selected'
    else if key == "s" || key == "\ESC[B" then do
        -- move down
        let selected' = (selected + 1) `mod` numOptions
        menuController' prompt options selected'
    else if key == " " || key == "\n" then do
        -- select with space or enter
        return selected
    else do
        -- invalid key
        menuController' prompt options selected

-- | printOptions prints a list of options with the selected option highlighted
printOptions :: [String] -> Int -> IO ()
printOptions options selected = do
    let options' = zipWith (\i o -> if i == selected then "> " ++ o else "  " ++ o) [0..] options
    mapM_ putStrLn options'

helpMsg = "IN GAME CONTROLS:\n\
\* Use WASD or arrow keys to move\n\
\* Press 'R' or spacebar to reveal\n\
\* Press 'M' or 'X' to mark\n\
\* Press P or ESC to pause\n"

-- | pauseGame is a controller for the pause menu
gamePausedMsg = "===GAME PAUSED===\n"
pauseGame :: Model -> IO Model
pauseGame model = do
    clearScreen
    choice <- menuController gamePausedMsg ["Resume", "Help", "Quit", "Save", "Load", "Manage Save Data"]
    case choice of
        0 -> do
            hSetEcho stdin False
            hSetBuffering stdin NoBuffering
            return model
        1 -> do
            clearScreen
            putStrLn helpMsg
            putStrLn "Press any key to return to pause menu..."
            getKey
            pauseGame model
        2 -> do
            exit <- menuController "Are you sure you want to quit?" ["Yes", "No"]
            if exit == 0 then do
                putStrLn "Exiting game..."
                exitSuccess
            else do
                pauseGame model
        3 -> do
            saveGame (board model)
            pauseGame model
        4 -> do
            model' <- loadGame model
            pauseGame model'
        5 -> do
            manageSaveData
            pauseGame model
        _ -> do
            putStrLn "Invalid action!" -- should never happen
            pauseGame model

-- Save Data Management --
saveDataDir = "saveData"

writeBoardToFile :: Board -> String -> IO ()
writeBoardToFile board name = do
    B.writeFile name (encode board)

readBoardFromFile :: String -> IO Board
readBoardFromFile name = do
    file <- B.readFile name
    case decode file of
        Nothing -> error $ "Error loading game from " ++ name ++ "!"
        Just b -> return b

-- | manageSaveData allows users to rename and delete save files
manageSaveData :: IO ()
manageSaveData = do
    clearScreen
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    
    putStrLn "Save files:"
    files <- getDirectoryContents saveDataDir
    let saveFiles = filter (\f -> take 1 f /= ".") files
    selection <- menuController "Select a save file to manage:" (saveFiles ++ ["Exit Manager"])
    if selection == length saveFiles then do
        putStrLn "Manage cancelled!"
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        return ()
    else do
        let name = saveFiles !! selection
        let filePath = saveDataDir ++ "/" ++ name
        exists <- doesFileExist filePath
        if exists then do
            manageSaveDataFile filePath name
            manageSaveData 
        else do
            putStrLn "File does not exist!"
            putStrLn "Exit save data manager? (y/n)"
            exit <- getLine
            if exit == "y" then do
                putStrLn "Exiting save data manager..."
                hSetEcho stdin False
                hSetBuffering stdin NoBuffering
                return ()
            else do
                hSetEcho stdin False
                hSetBuffering stdin NoBuffering
                manageSaveData
                return ()

manageSaveDataFile :: String -> String -> IO ()
manageSaveDataFile filePath name = do
    -- prompt user for action
    selection <- menuController "Select an action:" ["Rename", "Delete", "Cancel"]
    case selection of
        0 -> do
            putStrLn "Enter a new name for the save file:"
            newName <- getLine
            let newFilePath = saveDataDir ++ "/" ++ newName
            renameFile filePath newFilePath
            putStrLn $ "Successfully renamed \"" ++ name ++ "\" to \"" ++ newName ++ "\"!"
            return ()
        1 -> do
            delete <- menuController "Are you sure you want to delete this save file?" ["Yes", "No"]
            if delete == 0 then do
                removeFile filePath
                putStrLn $ "Successfully deleted \"" ++ name ++ "\"!"
            else do
                putStrLn "Delete cancelled!"
            return ()
        2 -> do
            putStrLn "Manage cancelled!"
        _ -> do
            putStrLn "Invalid action!"
            putStrLn "Manage cancelled!"

saveGame :: Board -> IO ()
saveGame board = do
    clearScreen
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    putStrLn "Enter a name for your save file:"
    name <- getLine
    if name == "" then do
        putStrLn "Invalid name!"
        saveGame board
    else do 
        let filePath = saveDataDir ++ "/" ++ name
        exists <- doesFileExist filePath
        if exists then do
            overwrite <- menuController ("File " ++ name ++ " already exists!\nOverwrite?") ["Yes", "No"]
            if overwrite == 0 then do
                writeBoardToFile board filePath
                clearScreen
                putStrLn $ "Successfully saved game to file \"" ++ name ++ "\"!"
            else do
                clearScreen
                putStrLn "Save cancelled!"
        else do
            writeBoardToFile board filePath
            clearScreen
            putStrLn $ "Successfully saved game to file \"" ++ name ++ "\"!"
    
    putStrLn "Press any key to continue..."
    getKey
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    return ()

loadGame :: Model -> IO Model
loadGame prevModel = do
    clearScreen
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    -- get all save files
    putStrLn "Save files:"
    files <- getDirectoryContents saveDataDir
    let saveFiles = filter (\f -> take 1 f /= ".") files
    choice <- menuController "Select a save file to load:" (saveFiles ++ ["Cancel"])
    if choice == length saveFiles then do
        putStrLn "Load cancelled!"
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        return prevModel
    else do
        let name = saveFiles !! choice
        let filePath = saveDataDir ++ "/" ++ name
        exists <- doesFileExist filePath
        if not exists then do
            putStrLn "File does not exist!"
            exit <- menuController "File does not exist! Exit save data manager?" ["Yes", "No"]
            if exit == 0 then do
                putStrLn "Exiting save data manager..."
                hSetEcho stdin False
                hSetBuffering stdin NoBuffering
                return prevModel
            else do 
                hSetEcho stdin False
                hSetBuffering stdin NoBuffering
                loadGame prevModel
        else do
            board <- readBoardFromFile filePath
            putStrLn "Load Successful!"
            putStrLn "Press any key to continue..."
            return (Model board (0, 0))
    


playMove :: Move -> Model -> Model
playMove move model = do
    let board' = play move (board model)
    Model board' (cursor model)

moveCursor :: Model -> Int -> Int -> Model
moveCursor model dr dc = do
    let numRows = nr (board model)
        numCols = nc (board model)
        r' = (fst (cursor model) + dr) `mod` numRows
        c' = (snd (cursor model) + dc) `mod` numCols
        cursor' = (r', c')
    Model (board model) cursor'


-- Displaying the Game --
yellowEscSeq = "\x1b[30;43m"
endEscSeq = "\x1b[0m"
charsPerCell = 3

printModel :: Model -> IO ()
printModel model = do
    let boardStr = show (board model)
        boardStrLen = length boardStr
        numRows = nr (board model)
        numCols = nc (board model)
        charsPerLine = numCols * charsPerCell + 1 -- +1 for newline
        cursorPos = cursor model
        -- given cursorPos = (r, c):
        -- cursorStart = r * charsPerLine + c * 3
        -- cursorEnd = cursorStart + 3
        cursorStart =  fst cursorPos * charsPerLine + snd cursorPos * charsPerCell
        cursorEnd = cursorStart + 3
    
    putStrLn $ "BOMBS LEFT: " ++ show (numBombs (board model) - numMarked (board model))
    putStrLn $ formatBoardString boardStr cursorStart cursorEnd
    
formatBoardString :: String -> Int -> Int -> String
formatBoardString bs start end = do
    let (before, after) = splitAt start bs
        (cursor, rest) = splitAt (end - start) after
    before ++ yellowEscSeq ++ cursor ++ endEscSeq ++ rest

module Board where

import Data.List ( filter, foldl', take, nub )
import Data.Set
import System.Random
import Debug.Trace ( trace )
type Position = (Int, Int)

-- | Get all adjacent positions.
-- | does not check if the positions is on the board
adjacent :: Position  -> [Position]
adjacent (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

-- | Board data structure.
data Board = Board {
    width :: Int,
    height :: Int,
    bombs :: Set Position,
    revealed :: Set Position,
    marked :: Set Position
}

-- | Initialize an empty board with the given dimensions.
initBoard :: Int -> Int -> Board
initBoard width height = Board {
    width = width,
    height = height,
    bombs = empty,
    revealed = empty,
    marked = empty
}

instance Show Board where
    show :: Board -> String
    show board = unlines $ [showRow y | y <- [0..height board - 1]]
        where
            showRow :: Int -> String
            showRow y = concat [showPosition (x, y) | x <- [0..width board - 1]]
            showPosition :: Position -> String
            showPosition pos
                | pos `Data.Set.member` revealed board = showRevealed pos
                | pos `Data.Set.member` marked board = showMarked pos
                | otherwise = showHidden pos
            showRevealed :: Position -> String
            showRevealed pos
                | pos `Data.Set.member` bombs board = "[*]"
                | otherwise = showAdjacentBombs pos
            showMarked :: Position -> String
            showMarked _ = "[X]"
            showHidden :: Position -> String
            showHidden _ = "[-]"
            showAdjacentBombs :: Position -> String
            showAdjacentBombs pos = case numAdjacentBombs pos board of
                n -> "[" ++ show n ++ "]"
         

-- | Set a single bomb at the given position.
-- | This function is idempotent.
setBomb :: Position -> Board -> Board
setBomb pos board = board {
    bombs = Data.Set.insert pos (bombs board)
}

-- | Returns the number of bombs adjacent to the given position.
numAdjacentBombs :: Position -> Board -> Int
numAdjacentBombs pos board = length $ Data.List.filter (`Data.Set.member` bombs board) $ adjacent pos

-- | Generate n random positions for bombs
randomPositions :: Int -> Int -> Int -> IO [Position]
randomPositions width height numBombs = 
    let positions = [(x, y) | x <- [0..width - 1], y <- [0..height - 1]]
    in do
        gen <- getStdGen
        return $ Data.List.take numBombs $ nub $ randomRs (0, length positions - 1) gen >>= return . (positions !!)

-- | Set bombs at the given positions.
setBombs :: [Position] -> Board -> Board
setBombs positions board = Data.List.foldl' (flip setBomb) board positions

setNRandomBombs :: Int -> Board -> IO Board
setNRandomBombs numBombs board = do
    positions <- randomPositions (width board) (height board) numBombs
    trace (show positions) $ return $ setBombs positions board


-- Game State Implementations --

-- | Returns true if the given position is revealed.
isRevealed :: Position -> Board -> Bool
isRevealed pos board = pos `Data.Set.member` revealed board

-- | Returns true if the given position is marked.
isMarked :: Position -> Board -> Bool
isMarked pos board = pos `Data.Set.member` marked board

-- Returns true if the given position is a bomb.
isBomb :: Position -> Board -> Bool
isBomb pos board = pos `Data.Set.member` bombs board

-- | Returns true if the game is lost.
-- the game is lost if any bomb is revealed
isGameLost :: Board -> Bool
isGameLost board = Data.Set.intersection (bombs board) (revealed board) /= empty

-- | Returns true if the game is won.
-- The game is won if all bombs are marked.
isGameWon :: Board -> Bool
isGameWon board = Data.Set.isSubsetOf (bombs board) (marked board)

-- Move Implementations --
data Move = Reveal Position | Mark Position | Unmark Position
    
-- | play a move on the board
play :: Move -> Board -> Board
play (Reveal pos) = revealPosition pos
play (Mark pos) = markPosition pos
play (Unmark pos) = unmarkPosition pos

-- | Reveal the given position.
-- If the position is not on the board, do nothing.
-- If the position is already revealed, do nothing.
-- If the position is marked, do nothing.
-- If the position is a bomb, reveal only the given position. (Game over)
-- Otherwise, reveal only the given position.
revealPosition :: Position -> Board -> Board
revealPosition pos board
    | fst pos < 0 || fst pos >= width board || snd pos < 0 || snd pos >= height board = board
    | pos `Data.Set.member` revealed board = board
    | pos `Data.Set.member` marked board = board
    | pos `Data.Set.member` bombs board = board
    | otherwise = board {
        revealed = Data.Set.insert pos (revealed board)
    }

-- given a position with no adjacent bombs, recursively get all adjacent positions with no adjacent bombs
-- if a position does have a bomb adjacent, do not get any of its adjacent positions
getAllSafeAdjacentPositions :: Position -> Board -> [Position]
getAllSafeAdjacentPositions pos board
    | numAdjacentBombs pos board /= 0 = []
    | otherwise = pos : concat [getAllSafeAdjacentPositions pos' board | pos' <- adjacent pos, pos' `Data.Set.notMember` revealed board]

-- | Mark the given position.
-- | If the position is not on the board, do nothing.
-- | If the position is already marked, do nothing.
-- | If the position is revealed, do nothing.
-- | Otherwise, mark the position.
markPosition :: Position -> Board -> Board
markPosition pos board
    | pos `Data.Set.member` marked board = board
    | pos `Data.Set.member` revealed board = board
    | otherwise = board {
        marked = Data.Set.insert pos (marked board)
    }

-- | Unmark the given position.
-- | If the position is not marked, do nothing.
-- | Otherwise, unmark the position.
unmarkPosition :: Position -> Board -> Board
unmarkPosition pos board
    | pos `Data.Set.member` marked board = board {
        marked = Data.Set.delete pos (marked board)
    }
    | otherwise = board

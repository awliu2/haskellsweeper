{-# LANGUAGE OverloadedStrings #-} 
module Board where
import Data.List ( filter, foldl', take, nub, concatMap )
import Data.Set as Set
import System.Random ( getStdGen, Random(randomRs) )
import Control.Applicative
import Control.Monad
import Data.Aeson 
import Debug.Trace ( trace )

type Position = (Int, Int)

-- | Get a list of all adjacent positions.
-- | does not check if the positions is on the board
adjacent :: Position  -> Set Position 
adjacent (r, c) = Set.fromList [(r + dr, c + dc) | dr <- [-1..1], dc <- [-1..1], dr /= 0 || dc /= 0]

{-# LANGUAGE OverloadedStrings #-}
-- | Board data structure.
data Board = Board {
    nr :: Int,
    nc :: Int,
    bombs :: Set Position,
    revealed :: Set Position,
    marked :: Set Position
} deriving (Eq)

instance FromJSON Board where
    parseJSON = withObject "Board" $ \v -> Board
        <$> v .: "nr"
        <*> v .: "nc"
        <*> v .: "bombs"
        <*> v .: "revealed"
        <*> v .: "marked"

instance ToJSON Board where
    toJSON (Board nr nc bombs revealed marked) =
        object [ "nr" .= nr
               , "nc" .= nc
               , "bombs" .= bombs
               , "revealed" .= revealed
               , "marked" .= marked
               ]
    toEncoding (Board nr nc bombs revealed marked) =
        pairs $  "nr" .= nr
               <> "nc" .= nc
               <> "bombs" .= bombs
               <> "revealed" .= revealed
               <> "marked" .= marked

-- | Initialize an empty board with the given dimensions.
initBoard :: Int -> Int -> Board
initBoard nr nc = Board {
    nr = nr,
    nc = nc,
    bombs = Set.empty,
    revealed = Set.empty,
    marked = Set.empty
}

instance Show Board where
    show :: Board -> String
    show board = unlines $ [showRow r | r <- [0..nr board - 1]]
        where
            showRow :: Int -> String
            showRow r = concat [showPosition (r, c) | c <- [0..nc board - 1]]
            showPosition :: Position -> String
            showPosition pos
                | isRevealed pos board = showRevealed pos
                | isMarked pos board = showMarked pos
                | otherwise = showHidden pos
            showRevealed :: Position -> String
            showRevealed pos
                | pos `Set.member` bombs board = "[*]"
                | otherwise = showAdjacentBombs pos
            showMarked :: Position -> String
            showMarked _ = "[X]"
            showHidden :: Position -> String
            showHidden _ = "[ ]"
            showAdjacentBombs :: Position -> String
            showAdjacentBombs pos = case numAdjacentBombs pos board of
                0 -> "   "
                n -> " " ++ show n ++ " "
         

-- | Set a single bomb at the given position.
-- | This function is idempotent.
setBomb :: Position -> Board -> Board
setBomb pos board = board {
    bombs = Set.insert pos (bombs board)
}

-- | Returns the number of bombs adjacent to the given position.
numAdjacentBombs :: Position -> Board -> Int
numAdjacentBombs pos board = Set.size $ Set.filter (`Set.member` bombs board) $ adjacent pos

-- | Generate n random positions for bombs
randomPositions :: Int -> Int -> Int -> IO [Position]
randomPositions nr nc numBombs = 
    let positions = [(x, y) | x <- [0..nr - 1], y <- [0..nc - 1]]
    in do
        gen <- getStdGen
        return $ Data.List.take numBombs $ nub $ randomRs (0, length positions - 1) gen >>= return . (positions !!)

-- | Set bombs at the given positions.
setBombs :: [Position] -> Board -> Board
setBombs positions board = Data.List.foldl' (flip setBomb) board positions

setNRandomBombs :: Int -> Board -> IO Board
setNRandomBombs numBombs board = do
    positions <- randomPositions (nr board) (nc board) numBombs
    return $ setBombs positions board

-- Game State Implementations --

-- | Returns the number of currently marked squares
numMarked :: Board -> Int
numMarked board = Set.size (marked board)

-- | Returns the number of bombs on the board
numBombs :: Board -> Int
numBombs board = Set.size (bombs board)

-- | Returns true if the given position is revealed.
isRevealed :: Position -> Board -> Bool
isRevealed pos board = pos `Set.member` revealed board

-- | Returns true if the given position is marked.
isMarked :: Position -> Board -> Bool
isMarked pos board = pos `Set.member` marked board

-- | Returns true if the given position is a bomb.
isBomb :: Position -> Board -> Bool
isBomb pos board = pos `Set.member` bombs board

-- | Returns true if the given position is on the board.
isOnBoard :: Position -> Board -> Bool
isOnBoard pos board = fst pos >= 0 && fst pos < nr board && snd pos >= 0 && snd pos < nc board

-- | Returns true if the game is lost.
-- the game is lost if any bomb is revealed
isGameLost :: Board -> Bool
isGameLost board = Set.intersection (bombs board) (revealed board) /= Set.empty

-- | Returns true if the game is won.
-- The game is won if all bombs are marked.
isGameWon :: Board -> Bool
isGameWon board = Set.isSubsetOf (bombs board) (marked board)

-- Move Implementations --
data Move = Reveal Position | Mark Position deriving (Eq, Show)
    
-- | play a move on the board
play :: Move -> Board -> Board
play (Reveal pos) b = revealPosition False pos b
play (Mark pos) b = if isMarked pos b then unmarkPosition pos b else markPosition pos b

-- | Reveal the given position.
-- If the position is not on the board, do nothing.
-- If the position is already revealed, this reveals all adjacent positions that are not marked (which could be bombs)
-- If the position is marked, do nothing.
-- If the position is a bomb, reveal only the given position. (Game over)
-- Otherwise, reveal only the given position.
revealPosition :: Bool -> Position -> Board -> Board
revealPosition ignoreBombs pos board 
    | not $ isOnBoard pos board = board
    | isRevealed pos board && not ignoreBombs = do
        -- reveal all adjacent positions that are not marked
        let adjacentPositions = Set.filter (\p -> not (isRevealed p board) && not (isMarked p board) && isOnBoard p board) (adjacent pos)
        -- if any of the adjacent positions are bombs, reveal all bombs
        trace (show adjacentPositions) $ if Set.size (Set.intersection adjacentPositions (bombs board)) > 0 then revealAllBombs board
        else do 
        -- otherwise, reveal all adjacent positions while ignoring bombs that are adjacent to the adjacent positions
         Set.foldl' (flip (revealPosition True)) board adjacentPositions
    | isMarked pos board = board
    | pos `Set.member` bombs board = if ignoreBombs then board else revealAllBombs board
    | otherwise = revealRecursive pos board 
-- | Reveal just given position.
revealOnlyPosition :: Position -> Board -> Board
revealOnlyPosition pos board = board {
    revealed = Set.insert pos (revealed board)  
}


-- | Reveal the given position and all adjacent positions with no adjacent bombs.
revealRecursive :: Position -> Board -> Board
revealRecursive pos board = board {
    revealed = Set.insert pos (Set.union (revealed board) (getAllSafeAdjacentPositions pos board Set.empty))
}

-- | Given a position with no adjacent bombs, recursively get all adjacent positions with no adjacent bombs
-- the positions must not be revealed, marked, or bombs and the positions must be on the board
getAllSafeAdjacentPositions :: Position -> Board -> Set Position -> Set Position
getAllSafeAdjacentPositions pos board acc = do
    if numAdjacentBombs pos board > 0 then acc else do
        let adjacentPositions = Set.filter (\p -> not (isRevealed p board) && not (isMarked p board) && not (isBomb p board) && isOnBoard p board) (adjacent pos)
        let newAcc = acc `union` adjacentPositions
        if Set.size newAcc == Set.size acc then acc else Set.foldl' (\acc' p -> getAllSafeAdjacentPositions p board acc') newAcc adjacentPositions

-- | Reveal all bombs.
--  This is called when the player loses.
revealAllBombs :: Board -> Board
revealAllBombs board = board {  
    revealed = Set.union (bombs board) (revealed board)
}


-- | Mark the given position.
-- | If the position is not on the board, do nothing.
-- | If the position is already marked, do nothing.
-- | If the position is revealed, do nothing.
-- | Otherwise, mark the position.
markPosition :: Position -> Board -> Board
markPosition pos board
    | pos `Set.member` marked board = board
    | pos `Set.member` revealed board = board
    | otherwise = board {
        marked = Set.insert pos (marked board)
    }

-- | Unmark the given position.
-- | If the position is not marked, do nothing.
-- | Otherwise, unmark the position.
unmarkPosition :: Position -> Board -> Board
unmarkPosition pos board
    | pos `Set.member` marked board = board {
        marked = Set.delete pos (marked board)
    }
    | otherwise = board

    

module Main where

import Board

data Action = KeyPress Char



main :: IO ()
main = do
    print "Hello, World!"
    let board = initBoard 10 10
    board' <- setNRandomBombs 10 board
    print ("Bombs " ++ show (bombs board'))
    -- play a move
    print board'
    let board'' = play (Reveal (0, 0)) board
    print board''

    -- play another move
    let board''' = play (Mark (0, 1)) board''
    print board'''


    

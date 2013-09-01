module Main where

import Control.Monad (forever)

import Data.Array (elems)
import Data.List (concatMap, intercalate)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Text.Read (readMaybe)

import TicTacToe ( GameState(..)
                 , Board
                 , Square
                 , newGame
                 , (/?/)
                 , nextGameState)

main :: IO ()
main = forever $ do
    putStrLn "=========="
    putStrLn "NEW GAME"
    putStrLn "=========="
    putStrLn . printGameState $ newGame
    runGame newGame

runGame :: GameState -> IO ()
runGame gs = do
    putStrLn "Enter position in format (x,y):"
    pos <- fmap (swap . fromMaybe (-1, -1) . readMaybe) getLine
    case gs /?/ pos of
        Just gs' -> case nextGameState gs' of
                        gs''@(InProgress _ _) -> do
                            putStrLn "=========="
                            putStrLn . printGameState $ gs''
                            runGame gs''
                        gs'' -> do
                            putStrLn . printBoard . board $ gs'
                            putStrLn . printGameState $ gs''
        Nothing -> do putStrLn "Invalid position."
                      runGame gs

printGameState :: GameState -> String
printGameState (InProgress player board) = "Player: " ++ show player ++ "\n"
                                        ++ " Board:\n" ++ printBoard board
printGameState Draw    = "The game ended in a draw."
printGameState (Won p) = "The game was won by player " ++ show p ++ "."

printBoard :: Board -> String
printBoard = intercalate "\n" . chunksOf 3 . concatMap printSquare . elems

printSquare :: Square -> String
printSquare Nothing        = " "
printSquare (Just player)  = show player

module Main where

import Control.Monad (forever)
import Data.Array (elems)
import Data.List (concatMap, intercalate)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Text.Read (readMaybe)
import TicTacToe ( GameState(..)
                 , Square
                 , newGame
                 , (/?/)
                 , nextGameState)

main :: IO ()
main = forever $ do
    putStrLn "=========="
    putStrLn "NEW GAME"
    runGame newGame

runGame :: GameState -> IO ()
runGame gs@(InProgress player board) = do
    putStrLn "=========="
    putStrLn . ppGameState $ gs
    putStrLn "Enter position:"
    pos <- fmap (fromMaybe (-1, -1) . readMaybe) getLine
    case board /?/ pos $ player of
        Just board' -> case nextGameState $ gs {board = board'} of
            Won p -> putStrLn $ "Player " ++ show p ++ " wins."
            Draw  -> putStrLn "The game ended in a draw."
            gs    -> runGame gs
        Nothing -> putStrLn "Invalid position." >> runGame gs

ppGameState :: GameState -> String
ppGameState (InProgress player board) =
        "Player: " ++ show player ++ "\n"
     ++ " Board:\n"
     ++ (intercalate "\n" . chunksOf 3 . concatMap ppSquare . elems $ board)

ppSquare :: Square -> String
ppSquare Nothing = " "
ppSquare (Just player)  = show player

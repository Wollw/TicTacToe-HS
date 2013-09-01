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
    putStrLn "=========="
    putStrLn . printGameState $ newGame
    runGame newGame

runGame :: GameState -> IO ()
runGame gs@(InProgress player board) = do
    putStrLn "Enter position in format (x,y):"
    pos <- fmap (swap . fromMaybe (-1, -1) . readMaybe) getLine
    case board /?/ pos $ player of
        Just board' -> let gs' = gs {board = board'}
                       in do putStrLn "=========="
                             putStrLn . printGameState $ gs'
                             case nextGameState gs' of
                                 gs''@(InProgress _ _) -> runGame gs''
                                 gs'' -> putStrLn . printGameState $ gs''
        Nothing -> do putStrLn "Invalid position."
                      runGame gs

printGameState :: GameState -> String
printGameState (InProgress player board) =
        "Player: " ++ show player ++ "\n"
     ++ " Board:\n"
     ++ (intercalate "\n" . chunksOf 3 . concatMap printSquare . elems $ board)
printGameState Draw    = "The game ended in a draw."
printGameState (Won p) = "The game was won by player " ++ show p ++ "."

printSquare :: Square -> String
printSquare Nothing = " "
printSquare (Just player)  = show player

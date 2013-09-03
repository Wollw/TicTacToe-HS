module Main where

import Control.Monad (forever)

import Data.Array (elems)
import Data.List (intersperse)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)

import System.Exit (exitSuccess)

import Text.Read (readMaybe)

import Game.TicTacToe ( GameState(..)
                      , Position
                      , Board
                      , Square
                      , newGame
                      , (/?/)
                      , nextGameState)

data Command = Invalid
             | Quit
             | Place Position deriving Show

main :: IO ()
main = forever $ do
    putStrLn "=========="
    putStrLn "NEW GAME"
    putStrLn "=========="
    putStrLn . printGameState $ newGame
    runGame newGame

runGame :: GameState -> IO ()
runGame gs = do
    putStrLn $ "Enter position for player "
            ++ (show . player) gs
            ++ " in format (x,y) or 'q' to quit:"
    command <- getCommand
    case command of
        Quit      -> putStrLn "Quitting..." >> exitSuccess
        Invalid   -> putStrLn "Invalid command." >> runGame gs
        Place pos -> case gs /?/ pos of
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

getCommand :: IO Command
getCommand = do
    commandString <- getLine
    return $ case readMaybe commandString of
        Just pos  -> Place $ swap pos
        _         -> case commandString of
            "q" -> Quit
            _   -> Invalid

printGameState :: GameState -> String
printGameState (InProgress p b) = unlines [ "Player: " ++ show p
                                          , " Board:", printBoard b
                                          ]
printGameState Draw    = "The game ended in a draw."
printGameState (Won p) = "The game was won by player " ++ show p ++ "."

printBoard :: Board -> String
printBoard b = unlines . intersperse "-----" $ map (intersperse '|') rows
  where rows = chunksOf 3 . concatMap printSquare . elems $ b

printSquare :: Square -> String
printSquare Nothing        = " "
printSquare (Just p) = show p

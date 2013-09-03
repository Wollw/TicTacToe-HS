{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.IORef (readIORef, writeIORef, newIORef)
import Data.Array (assocs)

import Graphics.UI.FreeGame

import TicTacToe ( GameState(..)
                 , Position
                 , Player (..)
                 , newGame
                 , nextGameState
                 , (/?/)
                 , inProgress
                 )

width, height :: Num a => a
width  = 512
height = 512

gameConfiguration = def { _windowSize  = V2 width height
                        , _windowTitle = "TicTacToe"
                        }


loadBitmaps "../res/img/"

main = runGame gameConfiguration $ do
    mouseDownRef  <- newIORef' False
    gameStateRef  <- newIORef' newGame
    font <- loadFont "res/font/VL-PGothic-Regular.ttf"
    forever $ do
        
        -- Draw the background
        translate center $ fromBitmap _background_png

        -- Game Logic
        gameState <- readIORef' gameStateRef
        case gameState of
            gs@(InProgress player board) -> do
                -- Handle new mouse input
                mouseDownPrev <- readIORef' mouseDownRef
                mouseDownNow  <- mouseButtonL
                when (inProgress gs && not mouseDownPrev && mouseDownNow) $ do
                    clickPosition <- mousePosition
                    case gs /?/ positionToCoordinate clickPosition of
                        Just gs' -> writeIORef' gameStateRef $ nextGameState gs'
                        Nothing  -> return ()
                writeIORef' mouseDownRef mouseDownNow
                
                -- Draw the game
                translate center $ fromBitmap _border_png
                sequence_ [ drawSquare (coordinateToPosition coord) square
                          | (coord, square) <- assocs board ]
            gs  -> do
                gameOver gs font
                restartPressed <- keyChar 'R' 
                when restartPressed $ writeIORef' gameStateRef newGame

        -- Quit if 'q' is pressed
        quitPressed <- keyChar 'Q'
        when quitPressed quit

        tick
  where
    center = V2 (width / 2) (height / 2)
    drawSquare pos square = case square of
        Nothing  -> return ()
        Just X -> translate pos $ fromBitmap _playerx_png
        Just O -> translate pos $ fromBitmap _playero_png
    gameOver gs font = translate center
                         $ colored black
                         $ text font 17
                         $ message gs ++ "\nPress 'q' to quit"
                                      ++ "\nor 'r' to restart."
    message Draw    = "Draw!"
    message (Won p) = "Player "++show p++" wins!"

positionToCoordinate :: V2 Float -> Position
positionToCoordinate (V2 x y) = ( ceiling $ x / (width  / 3)
                                , ceiling $ y / (height / 3) )

coordinateToPosition :: Position -> V2 Float
coordinateToPosition (x, y) = V2 ( (width  / 2) + (fromIntegral x - 2) * (width  / 3))
                                 ( (height / 2) + (fromIntegral y - 2) * (height / 3))

newIORef'           = embedIO . newIORef
readIORef'          = embedIO . readIORef
writeIORef' ref     = embedIO . writeIORef ref

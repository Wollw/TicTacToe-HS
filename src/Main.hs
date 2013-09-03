{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Array (assocs)

import Graphics.UI.FreeGame

import Game.TicTacToe ( GameState(..)
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

gameConfiguration :: GUIParam
gameConfiguration = def { _windowSize  = V2 width height
                        , _windowTitle = "TicTacToe"
                        }

loadBitmaps "../res/img/"

main :: IO (Maybe a)
main = runGame gameConfiguration $ do
    mouseDownRef  <- newIORef' False
    gameStateRef  <- newIORef' newGame
    font <- loadFont "res/font/VL-PGothic-Regular.ttf"
    forever $ do
        
        -- Draw the background
        translate center $ fromBitmap _background_png

        -- Game Logic
        gameState <- readIORef' gameStateRef
        if inProgress gameState
          then do
                -- Handle new mouse input
                mouseDownPrev <- readIORef' mouseDownRef
                mouseDownNow  <- mouseButtonL
                when (not mouseDownPrev && mouseDownNow) $ do
                    clickLocation <- mousePosition
                    case gameState /?/ coordinateToPosition clickLocation of
                        Just gs' -> writeIORef' gameStateRef $ nextGameState gs'
                        Nothing  -> return ()
                writeIORef' mouseDownRef mouseDownNow
                
                -- Draw the game
                translate center $ fromBitmap _border_png
                sequence_ [ drawSquare (positionToCoordinate coord) square
                          | (coord, square) <- assocs . board $ gameState]
          else do gameOver gameState font
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
    message _       = "" -- Shouldn't get here

-- | Converts a pixel location to a Square's position.
coordinateToPosition :: V2 Float -> Position
coordinateToPosition (V2 x y) = ( ceiling $ x / (width  / 3)
                                , ceiling $ y / (height / 3) )

-- | Converts a Square's position to a pixel location to draw a sprite.
positionToCoordinate :: Position -> V2 Float
positionToCoordinate (x, y) = V2 ( (width  / 2) + (fromIntegral x - 2) * (width  / 3))
                                 ( (height / 2) + (fromIntegral y - 2) * (height / 3))

-- | newIORef lifted to the Game monad
newIORef' :: a -> Game (IORef a)
newIORef' = embedIO . newIORef

-- | readIORef lifted to the Game monad
readIORef' :: IORef a -> Game a
readIORef' = embedIO . readIORef

-- | writeIORef lifted to the Game monad
writeIORef' :: IORef a -> a -> Game ()
writeIORef' ref = embedIO . writeIORef ref

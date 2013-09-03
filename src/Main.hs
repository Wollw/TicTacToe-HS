{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Array (assocs)

import Graphics.UI.FreeGame

import Game.TicTacToe ( GameState(..)
                      , Square
                      , Position
                      , Player (..)
                      , newGame
                      , nextGameState
                      , (/?/)
                      , inProgress
                      )

-- | The width and height of the screen.
width, height :: Num a => a
width  = 512
height = 512

-- | The center coordinate for the screen in pixels.
center :: V2 Float
center = V2 (width / 2) (height / 2)

-- | Configuration settings for free-game.
gameConfiguration :: GUIParam
gameConfiguration = def { _windowSize  = V2 width height
                        , _windowTitle = "TicTacToe"
                        }

-- | Load image resources.
loadBitmaps "../res/img/"

main :: IO (Maybe a)
main = runGame gameConfiguration $ do
    mouseDownRef  <- newIORef' False
    gameStateRef  <- newIORef' newGame
    font <- loadFont "res/font/VL-PGothic-Regular.ttf"
    forever $ do
        
        -- Draw the background
        translate center $ fromBitmap _background_png

        --
        -- Two paths based on game state.
        -- We run the normal game logic if the game is still
        -- in progress.  We display the game over screen if
        -- the game has ended.
        --
        gameState <- readIORef' gameStateRef
        if inProgress gameState
          then do --
                  -- Game play logic.
                  --
                  -- Here we check for new mouse clicks and
                  -- if the location is empty fill it with
                  -- the current player's piece.
                  --
                  mouseDownPrev <- readIORef' mouseDownRef
                  mouseDownNow  <- mouseButtonL
                  when (not mouseDownPrev && mouseDownNow) $ do
                      clickLocation <- mousePosition
                      case gameState /?/ coordinateToPosition clickLocation of
                          Just gs' -> writeIORef' gameStateRef $ nextGameState gs'
                          Nothing  -> return ()
                  writeIORef' mouseDownRef mouseDownNow
                  
                  -- Draw the board grid and pieces to the screen.
                  translate center $ fromBitmap _border_png
                  sequence_ [ drawSquare coord square
                            | (coord, square) <- assocs . board $ gameState]
          else do --
                  -- Game over logic.
                  --
                  -- Here we display the way in which the
                  -- game ended (a draw or a win).  We also
                  -- check for the restart command and restart
                  -- the game if it is given.
                  --
                  gameOver gameState font
                  'R' `isPressedThen` writeIORef' gameStateRef newGame

        -- The quit command 'q' can be given at
        -- any to time to quit the game.
        'Q' `isPressedThen` quit

        tick

-- | Evaluate a Game action if a character is pressed.
isPressedThen :: Char -> Game () -> Game ()
c `isPressedThen` f = keyChar c >>= flip when f

-- | Given a pixel location and a TicTacToe Square
--   draws the appropriate image to that location
drawSquare :: Position -> Square -> Game ()
drawSquare pos square = case square of
    Nothing  -> return ()
    Just X -> translate (positionToCoordinate pos) $ fromBitmap _playerx_png
    Just O -> translate (positionToCoordinate pos) $ fromBitmap _playero_png


-- | Displays the game over text and commands reminders.
gameOver :: GameState -> Font -> Game ()
gameOver gs font = translate center
                     $ colored black
                     $ text font 17
                     $ gameStatusString gs ++ "\nPress 'q' to quit"
                                           ++ "\nor 'r' to restart."

-- | Generates a string explaining the current game status.
gameStatusString :: GameState -> String
gameStatusString Draw             = "Draw!"
gameStatusString (Won p)          = "Player "++show p++" wins!"
gameStatusString (InProgress _ _) = "Game in progress."

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

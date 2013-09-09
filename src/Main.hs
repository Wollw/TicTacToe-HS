{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Array (assocs)
import qualified Data.Foldable as F (forM_, mapM_)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

import Game.TicTacToe ( GameState(..)
                      , Square
                      , Position
                      , Player (..)
                      , newGame
                      , nextGameState
                      , (/?/)
                      , inProgress
                      )

import Graphics.UI.FreeGame

import System.FilePath ((</>))

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
loadBitmaps "../res"

-- | Helper function for providing the player's piece image Bitmap.
playerImage :: Player -> Bitmap
playerImage X = _playerx_png
playerImage O = _playero_png

-- | Helper function for providing the player win image.
gameOverImage :: GameState -> Maybe Bitmap
gameOverImage (Won X) = Just _wonx_png
gameOverImage (Won O) = Just _wono_png
gameOverImage Draw    = Just _draw_png
gameOverImage _       = Nothing

-- | The Bitmap used for the lines between squares of the board.
borderImage :: Bitmap
borderImage = _border_png

-- | The Bitmap used for the background of the board.
--   Displayed behind the squares.
backgroundImage :: Bitmap
backgroundImage = _background_png

main :: IO (Maybe a)
main = runGame gameConfiguration $ do
    mouseDownRef <- newIORef' False
    gameStateRef <- newIORef' newGame
    forever $ do
        
        -- Draw the background
        translate center $ fromBitmap backgroundImage

        --
        -- Two paths based on game state.
        -- We run the normal game logic if the game is still
        -- in progress.  We display the game over screen if
        -- the game has ended.
        --
        gameState <- readIORef' gameStateRef
        mouseDownPrev <- readIORef' mouseDownRef
        mouseDownNow  <- mouseButtonL
        if inProgress gameState
          then do --
                  -- Game play logic.
                  --
                  -- Here we check for new mouse clicks and
                  -- if the location is empty fill it with
                  -- the current player's piece.
                  --
                  when (not mouseDownPrev && mouseDownNow) $
                    F.mapM_ (writeIORef' gameStateRef) -- save the update
                      <$> nextGameState'       -- produce the updated game state
                      =<< drawBoard'           -- display the intermediate board state
                      =<< (gameState /?/)      -- add piece to board
                      <$> coordinateToPosition -- board position
                      <$> mousePosition        -- pixel click position
                  
                  -- Draw the board grid and pieces to the screen.
                  translate center $ fromBitmap borderImage
                  drawBoard gameState
          else do --
                  -- Game over logic.
                  --
                  -- Here we display the way in which the
                  -- game ended (a draw or a win).  Starts a
                  -- new game if it detects a click.
                  --
                  gameOver gameState
                  when (not mouseDownPrev && mouseDownNow) $
                    writeIORef' gameStateRef newGame

        writeIORef' mouseDownRef mouseDownNow -- Update click state
        -- The quit command 'q' can be given at
        -- any to time to quit the game.
        'Q' `whenPressed` quit

        tick
  where
    nextGameState' = fmap nextGameState
    drawBoard' maybeGameState = F.mapM_ drawBoard maybeGameState
                             >> return maybeGameState

-- | Evaluate a Game action if a character is pressed.
whenPressed :: Char -> Game () -> Game ()
c `whenPressed` f = keyChar c >>= flip when f

-- | Given a pixel location and a TicTacToe Square
--   draws the appropriate image to that location
drawSquare :: Position -> Square -> Game ()
drawSquare pos square = F.forM_ square $
    translate (positionToCoordinate pos) . fromBitmap . playerImage


drawBoard :: GameState -> Game ()
drawBoard gs | inProgress gs = sequence_ [ drawSquare c s
                                         | (c, s) <- assocs . board $ gs ]
             | otherwise     = return ()

-- | Displays the game over text and commands reminders.
gameOver :: GameState -> Game ()
gameOver gs = F.mapM_ (translate center . fromBitmap) $ gameOverImage gs

-- | Converts a pixel location to a Square's position.
coordinateToPosition :: V2 Float -> Position
coordinateToPosition (V2 x y) = ( ceiling $ x / (width  / 3)
                                , ceiling $ y / (height / 3) )

-- | Converts a Square's position to a pixel location to draw a sprite.
positionToCoordinate :: Position -> V2 Float
positionToCoordinate (x, y) = V2 (( width/2)+(fromIntegral x-2)*( width/3))
                                 ((height/2)+(fromIntegral y-2)*(height/3))

-- | newIORef lifted to the Game monad
newIORef' :: a -> Game (IORef a)
newIORef' = embedIO . newIORef

-- | readIORef lifted to the Game monad
readIORef' :: IORef a -> Game a
readIORef' = embedIO . readIORef

-- | writeIORef lifted to the Game monad
writeIORef' :: IORef a -> a -> Game ()
writeIORef' ref = embedIO . writeIORef ref

{-# LANGUAGE TemplateHaskell #-}
module Main where

import Codec.Picture.Repa (decodeImageRGBA, imgData)

import Data.Array (assocs)
import qualified Data.Foldable as F (forM_, mapM_)
import Data.FileEmbed (embedDir)
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

-- | Used to load an embedded image file.
maybePicture :: Picture2D p => FilePath -> Maybe (p ())
maybePicture name = fromBitmap
                <$> toBitmap
                <$> imgData
                <$> (either fail return
                    =<< decodeImageRGBA
                    <$> lookup name $(embedDir "res"))

playerImage :: Player -> FilePath
playerImage X = "playerx.png"
playerImage O = "playero.png"

maybeGameOverImage :: GameState -> Maybe FilePath
maybeGameOverImage (Won X) = Just "wonx.png"
maybeGameOverImage (Won O) = Just "wono.png"
maybeGameOverImage Draw    = Just "draw.png"
maybeGameOverImage _       = Nothing

borderImage :: FilePath
borderImage = "border.png"

backgroundImage :: FilePath
backgroundImage = "background.png"

main :: IO (Maybe a)
main = runGame gameConfiguration $ do
    mouseDownRef <- newIORef' False
    gameStateRef <- newIORef' newGame
    previousGameStateRef <- newIORef' newGame
    forever $ do
        
        -- Draw the background
        translateMaybe center $ maybePicture "background.png"

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
                  writeIORef' previousGameStateRef gameState
                  when (not mouseDownPrev && mouseDownNow) $
                    saveGameState_ gameStateRef -- save the update
                      <$> nextGameState'        -- produce the updated game state
                      =<< drawBoard'            -- display the intermediate board state
                      =<< saveGameState previousGameStateRef -- save the current board state for drawing if game ends
                      =<< (gameState /?/)       -- add piece to board
                      <$> coordinateToPosition  -- board position
                      <$> mousePosition         -- pixel click position
                  
                  -- Draw the board grid and pieces to the screen.
                  translateMaybe center $ maybePicture borderImage
                  drawBoard gameState
          else do --
                  -- Game over logic.
                  --
                  -- Here we display the way in which the
                  -- game ended (a draw or a win).  Starts a
                  -- new game if it detects a click.
                  --
                  translateMaybe center $ maybePicture borderImage
                  drawBoard =<< readIORef' previousGameStateRef
                  drawGameOver gameState
                  when (not mouseDownPrev && mouseDownNow) $
                    writeIORef' gameStateRef newGame
        writeIORef' mouseDownRef mouseDownNow -- Update click state
        
        -- The quit command can be given at
        -- any to time to quit the game.
        KeyEsc `whenSpecialKeyPressed` quit

        tick
  where
    nextGameState' = fmap nextGameState
    drawBoard' maybeGameState = F.mapM_ drawBoard maybeGameState >> return maybeGameState
    saveGameState ref maybeGS = F.mapM_ (writeIORef' ref) maybeGS >> return maybeGS
    saveGameState_ ref maybeGS = void $ saveGameState ref maybeGS

-- | Convenience function for translating pictures that
--   are wrapped in a Maybe.
translateMaybe :: (Monad m, Picture2D m) => V2 Float -> Maybe (m a) -> m ()
translateMaybe pos = F.mapM_ $ translate pos

-- | Evaluate a Game action if a character is pressed.
whenCharPressed :: Char -> Game () -> Game ()
c `whenCharPressed` f = flip when f =<< keyChar c

-- | Evaluate a Game action if a character is pressed.
whenSpecialKeyPressed :: SpecialKey -> Game () -> Game ()
k `whenSpecialKeyPressed` f = flip when f =<< keySpecial k

-- | Given a pixel location and a TicTacToe Square
--   draws the appropriate image to that location
drawSquare :: Position -> Square -> Game ()
drawSquare pos square = F.forM_ square $
    translateMaybe (positionToCoordinate pos) . maybePicture . playerImage


-- | Draws all the squares of the board to the screen
--   if the game is in progress. Returns True if it
--   succeeds, False otherwise.
drawBoard :: GameState -> Game ()
drawBoard gs | inProgress gs = sequence_ [ drawSquare c s
                                         | (c, s) <- assocs . board $ gs ]
             | otherwise     = return ()

-- | Displays the game over text and commands reminders.
drawGameOver :: GameState -> Game ()
drawGameOver gs = F.mapM_ (translateMaybe center) $ maybePicture <$> maybeGameOverImage gs

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

{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Main where

import Codec.Picture.Repa (decodeImageRGBA, imgData)

import Control.Monad.Free (MonadFree)

import Data.Array (assocs)
import qualified Data.Foldable as F (Foldable, forM_, mapM_)
import Data.FileEmbed (embedDir)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

import Game.TicTacToe ( GameState(..)
                      , GamePhase(..)
                      , Square
                      , Position
                      , Player (..)
                      , newGame
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
maybeGameOverImage gs | phase gs == Won X = Just "wonx.png"
                      | phase gs == Won O = Just "wono.png"
                      | phase gs == Draw  = Just "draw.png"
                      | otherwise         = Nothing

borderImage :: FilePath
borderImage = "border.png"

backgroundImage :: FilePath
backgroundImage = "background.png"

main :: IO ()
main = void . runGame gameConfiguration $ do
  mouseDownRef <- newIORef' False
  gameStateRef <- newIORef' newGame
  forever $ do
    gameState     <- readIORef' gameStateRef
    mouseDownPrev <- readIORef' mouseDownRef
    mouseDownNow  <- mouseButtonL
    
    -- The quit command can be given at
    -- any to time to quit the game.
    KeyEsc `whenSpecialKeyPressed` quit

    --
    -- Process events on mouse clicks.
    -- If a mouse click occurred either
    -- attempt to place a new piece if the game
    -- is in progress or start a new game
    -- if the game is not in progress.
    --
    when (not mouseDownPrev && mouseDownNow) $
      if inProgress gameState
        then writeIORefF' gameStateRef -- save the update
             =<< (gameState /?/)       -- add piece to board
             <$> coordinateToPosition  -- board position
             <$> mousePosition         -- pixel click position
        else writeIORef' gameStateRef newGame -- start new game
    writeIORef' mouseDownRef mouseDownNow -- Update click state
    drawGameState gameState

    tick

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
drawSquare :: (Monad m, Picture2D m) => Position -> Square -> m ()
drawSquare pos square = F.forM_ square $
    translateMaybe (positionToCoordinate pos) . maybePicture . playerImage

-- | Draws all the squares of the board to the screen
--   if the game is in progress as well as the other
--   graphic elements.
drawGameState :: (Monad m, Picture2D m) => GameState -> m ()
drawGameState gs = do
    translateMaybe center $ maybePicture backgroundImage         -- draw background
    translateMaybe center $ maybePicture borderImage             -- draw border
    sequence_ [ drawSquare c s | (c, s) <- assocs . board $ gs ] -- draw pieces
    maybeDrawGameOver gs -- draw game over screen if game is over
  where
    maybeDrawGameOver gs = F.mapM_ (translateMaybe center) $ maybePicture <$> maybeGameOverImage gs

-- | Converts a pixel location to a Square's position.
coordinateToPosition :: V2 Float -> Position
coordinateToPosition (V2 x y) = ( ceiling $ x / (width  / 3)
                                , ceiling $ y / (height / 3) )

-- | Converts a Square's position to a pixel location to draw a sprite.
positionToCoordinate :: Position -> V2 Float
positionToCoordinate (x, y) = V2 (( width/2)+(fromIntegral x-2)*( width/3))
                                 ((height/2)+(fromIntegral y-2)*(height/3))

-- | newIORef lifted to the Game monad
newIORef' :: MonadFree (UI n) m => a -> m (IORef a)
newIORef' = embedIO . newIORef

-- | readIORef lifted to the Game monad
readIORef' :: MonadFree (UI n) m => IORef a -> m a
readIORef' = embedIO . readIORef

-- | writeIORef lifted to the Game monad
writeIORef' :: MonadFree (UI n) m => IORef a -> a -> m ()
writeIORef' ref = embedIO . writeIORef ref

-- | A variant of writeIORef for use on values in a
--   Foldable context.
writeIORefF :: F.Foldable f => IORef a -> f a -> IO ()
writeIORefF ref = F.mapM_ (writeIORef ref)

-- | writeIORefF lifted to the Game monad
writeIORefF' :: (F.Foldable f, MonadFree (UI n) m) => IORef a -> f a -> m ()
writeIORefF' ref = embedIO . writeIORefF ref

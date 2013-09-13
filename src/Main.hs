{-# LANGUAGE TemplateHaskell #-}

import Codec.Picture.Repa (decodeImageRGBA, imgData)

import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Free.Church (F)

import Data.Array (assocs)
import Data.FileEmbed (embedDir)
import qualified Data.Foldable as F (Foldable, forM_, mapM_)

import Game.TicTacToe

import Graphics.UI.FreeGame

-- | The width and height of the screen.
width, height :: Num a => a
width  = 512
height = 512

-- | Configuration settings for free-game.
gameConfig :: GUIParam
gameConfig = def { _windowSize  = V2 width height
                 , _windowTitle = "TicTacToe"
                 }

data FreeGameState = FreeGameState { _gameState     :: GameState
                                   , _mouseDownPrev :: Bool
                                   } deriving Show

main :: IO ()
main = void . runGameWithStateT gameConfig newFreeGameState $
    forever $ do
        freeGameState@(FreeGameState gameState mouseDownPrev) <- get
        mouseDownNow <- mouseButtonL
        if not mouseDownPrev && mouseDownNow
          then if inProgress gameState
                 then saveNewState mouseDownNow -- Save the updated state
                      =<< (gameState /?/)       -- Update the game state
                      <$> coordinateToPosition  -- Get square clicked
                      <$> mousePosition         -- Get position of click
                 else put $ FreeGameState newGame mouseDownNow -- Start a new game
          else put $ freeGameState {_mouseDownPrev = mouseDownNow} -- Update just mouse click
        drawGameState =<< _gameState <$> get
        tick
  where
    saveNewState mouse = F.mapM_ (\gs -> put $ FreeGameState gs mouse)

-- | Initial game state for a blank TicTacToe board.
newFreeGameState :: FreeGameState
newFreeGameState = FreeGameState newGame False

-- | Convenience function to combine runGame and runStateT
runGameWithStateT :: GUIParam -> b -> StateT b (F GUI) a -> IO (Maybe (a, b))
runGameWithStateT cfg state = runGame cfg . flip runStateT state

-- | The center coordinate for the screen in pixels.
center :: V2 Float
center = V2 (width / 2) (height / 2)

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

borderImage :: FilePath
borderImage = "border.png"

backgroundImage :: FilePath
backgroundImage = "background.png"

maybeGameOverImage :: GameState -> Maybe FilePath
maybeGameOverImage gs | phase gs == Won X = Just "wonx.png"
                      | phase gs == Won O = Just "wono.png"
                      | phase gs == Draw  = Just "draw.png"
                      | otherwise         = Nothing

-- | Converts a pixel location to a Square's position.
coordinateToPosition :: V2 Float -> Position
coordinateToPosition (V2 x y) = ( ceiling $ x / (width  / 3)
                                , ceiling $ y / (height / 3) )

-- | Converts a Square's position to a pixel location to draw a sprite.
positionToCoordinate :: Position -> V2 Float
positionToCoordinate (x,y) =
    V2 (( width / 2 ) + (fromIntegral x - 2) * ( width / 3))
       ((height / 2 ) + (fromIntegral y - 2) * (height / 3))

-- | Convenience function for translating pictures that
--   are wrapped in a Maybe.
translateMaybe :: (Monad m, Picture2D m) => V2 Float -> Maybe (m a) -> m ()
translateMaybe pos = F.mapM_ $ translate pos

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

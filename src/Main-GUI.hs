module Main where

import Control.Monad (forM_)
import qualified Graphics.UI.GLUT as GLUT (windowSize, get, Size(..))
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble, GLsizei)
import TicTacToe hiding (Position)

data PlaceEvent = PlaceEvent { player :: Player, position :: (Int, Int) }
    deriving Show
data GameAttribute = GameAttr GameState [PlaceEvent]

newGameState :: GameAttribute
newGameState = GameAttr newGame []

type TicTacToeObject = GameObject ()
type TicTacToeAction a = IOGame GameAttribute () () () a

width = 256
height = 256
w = fromIntegral width  :: GLdouble
h = fromIntegral height :: GLdouble

magenta = Just [(255, 0, 255)]

main :: IO ()
main = let winConfig = ((100,100), (width, height), "TicTacToe")
           bmpList   = [ ("res/border.bmp",  Nothing)
                       , ("res/playerX.bmp", magenta)
                       , ("res/playerO.bmp", magenta)
                       ]
           squares   = [ createSquare x y | x <- [0..2], y <- [0..2] ]
           objects   = [ objectGroup "squareGroup" squares ]
           gameMap   = textureMap 0 (w/3) (h/3) w h
           bindings  = [ (Char 'q', Press, \_ _ -> funExit)
                       , (MouseButton LeftButton, Press, onLeftMouseButtonPressed)
                       ]
       in do
        funInit winConfig gameMap objects () newGameState bindings gameCycle Idle bmpList

createSquare :: (Show a, Integral a) => a -> a -> GameObject ()
createSquare x y = let squarePic = Tex (w/3, h/3) 0
                       squarePos = (   w/6 + (fromIntegral x * w/3)
                                   , 5*h/6 - (fromIntegral y * h/3))
                   in object ("square" ++ show x ++ show y) squarePic False squarePos (0,0) ()

onLeftMouseButtonPressed :: Modifiers -> Position -> TicTacToeAction ()
onLeftMouseButtonPressed mods pos@(Position x y) = do
    (GameAttr gs eventList) <- getGameAttribute
    size <- getWindowSize
    liftIOtoIOGame $ putStrLn "click"
    setGameAttribute
        $ GameAttr gs $ eventList ++
        [PlaceEvent (TicTacToe.player gs) (squareCoord size (x,y))]
squareCoord (w,h) (x,y) = ( truncate $ fromIntegral x / (fromIntegral w / 3)
                          , truncate $ fromIntegral y / (fromIntegral h / 3)
                          )
squareName s p = "square" ++ showCoord (squareCoord s p)
showCoord (x,y) = show x ++ show y

-- Submit as patch?
getWindowSize :: IOGame t s u v (GLsizei,GLsizei)
getWindowSize = do 
    (GLUT.Size w h) <- liftIOtoIOGame . GLUT.get $ GLUT.windowSize
    return (w,h)

gameCycle :: TicTacToeAction ()
gameCycle = do
    (GameAttr gs eventList) <- getGameAttribute
    case gs of
        Won p      -> gameOver gs $ "Player " ++ show p ++ " wins!"
        Draw       -> gameOver gs "Draw!"
        InProgress _ board -> case eventList of
            [] -> return ()
            (PlaceEvent player (x,y)):es -> do
                size <- getWindowSize
                obj <- findObject (squareName size (x,y)) "squareGroup"
                case board /?/ (squareCoord size (x,y)) $ player of
                    Just board' -> do setGameAttribute $ GameAttr (nextGameState gs {board = board'}) (tail eventList)
                                      setObjectCurrentPicture (fromEnum player + 1) obj
                    Nothing -> return ()
            
  where
    gameOver gs str = printOnScreen str Fixed8By13 (w/2, h/2) 0 0 0
--    clearSquares = let objNames = [ "square" ++ show x ++ show y | x <- [0..2], y <- [0..2] ]
--                   in forM_ objNames $ \name -> do
--                        obj <- findObject name "squareGroup"
--                        setObjectCurrentPicture 0 obj

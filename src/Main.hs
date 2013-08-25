module Main where

import Control.Monad (when)
import qualified Graphics.UI.GLUT as GLUT (windowSize, get, Size(..))
import Graphics.UI.Fungen hiding (when)
import Graphics.Rendering.OpenGL (GLdouble, GLsizei)
import TicTacToe

data BoardState' = BoardState' { boardState :: Board
                             , currentPlayer  :: Player
                             }
data GameAttribute = BoardState BoardState'

newBoardState :: IO GameAttribute
newBoardState = do
    board <- newEmptyBoard
    return . BoardState $ BoardState' board X


type TicTacToeObject = GameObject ()
type TicTacToeAction a = IOGame GameAttribute () () () a

width = 256
height = 256
w = fromIntegral width  :: GLdouble
h = fromIntegral height :: GLdouble

magenta = Just [(255, 0, 255)]

main :: IO ()
main = let winConfig = ((0,0), (width, height), "TicTacToe")
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
        gameState <- newBoardState 
        funInit winConfig gameMap objects () gameState bindings gameCycle Idle bmpList

createSquare :: (Show a, Integral a) => a -> a -> GameObject ()
createSquare x y = let squarePic = Tex (w/3, h/3) 0
                       squarePos = (   w/6 + (fromIntegral x * w/3)
                                   , 5*h/6 - (fromIntegral y * h/3))
                   in object ("square" ++ show x ++ show y) squarePic False squarePos (0,0) ()

onLeftMouseButtonPressed :: Modifiers -> Position -> TicTacToeAction ()
onLeftMouseButtonPressed mods pos@(Position x y) = do
    size <- getWindowSize
    obj <- findObject (squareName size (x,y)) "squareGroup"

    (BoardState gs) <- getGameAttribute
    isValidCoord <- liftIOtoIOGame $ isValidCoordinate (boardState gs) $(squareCoord size (x,y))
    when isValidCoord $ do
        liftIOtoIOGame $ placePiece (boardState gs) (currentPlayer gs) (squareCoord size (x,y))
        setGameAttribute . BoardState $ gs { currentPlayer = if currentPlayer gs == X then O else X }
        setObjectCurrentPicture ((+1) . fromEnum . currentPlayer $ gs) obj
        liftIOtoIOGame $ placePiece (boardState gs) (currentPlayer gs) (squareCoord size (x,y))
  where
    squareCoord (w,h) (x,y) = ( truncate $ fromIntegral x / (fromIntegral w / 3)
                              , truncate $ fromIntegral y / (fromIntegral h / 3)
                              )
    squareName s p = "square" ++ (show . fst . squareCoord s) p
                              ++ (show . snd . squareCoord s) p

-- Submit as patch?
getWindowSize :: IOGame t s u v (GLsizei,GLsizei)
getWindowSize = do 
    (GLUT.Size w h) <- liftIOtoIOGame . GLUT.get $ GLUT.windowSize
    return (w,h)

gameCycle :: TicTacToeAction ()
gameCycle = do
    (BoardState gs) <- getGameAttribute
    squares <- liftIOtoIOGame . boardToSquares . boardState $ gs
    case gameState squares of
        Won p      -> do printOnPrompt $ "Player " ++ show p ++ " won!"
                         funExit
        Draw       -> do printOnPrompt "The game resulted in a draw!"
                         funExit
        InProgress -> return ()

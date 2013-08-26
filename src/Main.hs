module Main where

import qualified Graphics.UI.GLUT as GLUT (windowSize, get, Size(..))
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble, GLsizei)
import TicTacToe

data BoardState' = BoardState' { currentBoardState       :: Board
                               , currentPlayer    :: Player
                               , gameInProgress :: Bool
                               }
data GameAttribute = BoardState BoardState'

newBoardState :: IO GameAttribute
newBoardState = do
    board <- newEmptyBoard
    return . BoardState $ BoardState' board X True


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
    (BoardState gs) <- getGameAttribute
    when (gameInProgress gs) $ do
        size <- getWindowSize
        obj <- findObject (squareName size (x,y)) "squareGroup"

        isValidCoord <- liftIOtoIOGame $ isValidCoordinate (currentBoardState gs) $(squareCoord size (x,y))
        when isValidCoord $ do
            liftIOtoIOGame $ placePiece (currentBoardState gs) (currentPlayer gs) (squareCoord size (x,y))
            setGameAttribute . BoardState $ gs { currentPlayer = if currentPlayer gs == X then O else X }
            setObjectCurrentPicture ((+1) . fromEnum . currentPlayer $ gs) obj
            liftIOtoIOGame $ placePiece (currentBoardState gs) (currentPlayer gs) (squareCoord size (x,y))
  where
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
    (BoardState gs) <- getGameAttribute
    squares <- liftIOtoIOGame . boardToSquares . currentBoardState $ gs
    case gameState squares of
        Won p      -> gameOver gs $ "Player " ++ show p ++ " wins!"
        Draw       -> gameOver gs "Draw!"
        InProgress -> return ()
  where
    gameOver gs str = do clearSquares
                         printOnScreen str Fixed8By13 (w/2, h/2) 0 0 0
                         setGameAttribute $ BoardState $ gs {gameInProgress = False}

    clearSquares = let objNames = [ "square" ++ show x ++ show y | x <- [0..2], y <- [0..2] ]
                   in flip mapM_ objNames $ \name -> do
                        obj <- findObject name "squareGroup"
                        setObjectCurrentPicture 0 obj

module TicTacToe where

import Control.Monad (liftM)

import Data.Array.IO (IOArray)
import Data.Array.MArray (newListArray, readArray, writeArray, getElems)
import Data.List (find, intersperse, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

import qualified Graphics.UI.Fungen as FGE

import System.IO (hFlush, stdout)

import Text.Read (readMaybe)

-- | Storage for the board state.
type Board  = IOArray (Int, Int) Square

-- | Individual squares of the board are either empty ('Nothing')
-- | or a Player.
type Square = Maybe Player

-- | Players are represented as 'X' or 'O'.  This is used
-- | to maintain the game state while being played.
data Player  = X | O deriving (Show, Read, Eq, Enum)

-- | There are three possible states the game can be in.
-- | The game can either be won by a player, ended with a draw,
-- | or is still in progress.
data GameState = Won Player
               | Draw
               | InProgress
               deriving (Show, Read)

-- | One time setup and bootstrap for the game loop.
--main = do
--    putStr $  "TicTacToe!\n"
--           ++ "----------\n"
--           ++ "\n"
--    newEmptyBoard >>= loop X

-- | The main game loop.
loop :: Player -> Board -> IO ()
loop player board = do
    putStrLn =<< showBoard board
    coordinate <- getCoordinate
    putStrLn ""

    placePiece board player coordinate
    squares <- boardToSquares board
    case gameState squares of
        Won p -> do putStrLn =<< showBoard board 
                    putStrLn $ "Player " ++ show p ++ " won!"
        Draw  -> do putStrLn =<< showBoard board 
                    putStrLn "The game resulted in a draw!"
        InProgress -> loop player' board
  where
    player' = if player == X then O else X
    placePiece b p c = writeArray b c (Just p)
    getCoordinate :: IO (Int, Int)
    getCoordinate = do
        putStr $ show player ++ ": "
        hFlush stdout
        str <- getLine
        case readMaybe str of
            Just coord -> case coord <= (2,2) of
                True -> do
                    spaceState <- readArray board coord
                    case spaceState of
                        Nothing -> return coord
                        Just _  -> putStrLn "Square filled already." >> getCoordinate
                False -> putStrLn "Invalid Coordinate." >> getCoordinate
            Nothing -> putStrLn "Invalid Coordinate." >> getCoordinate

isValidCoordinate :: Board -> (Int,Int) -> IO Bool
isValidCoordinate board (x,y) = 
    case (x,y) <= (2,2) of
        True -> do spaceState <- readArray board (x,y)
                   case spaceState of
                       Nothing -> return True
                       Just _  -> return False
        False -> return False

placePiece b p c = writeArray b c (Just p)

-- | Creates a new empty board to play on.
newEmptyBoard :: IO Board
newEmptyBoard = newListArray ((0,0), (2,2)) $ replicate 9 Nothing

-- | Creates a list of lists of Squares from a Board used for printing
-- | the board state to the screen.
boardToSquares :: Board -> IO [[Square]]
boardToSquares b = liftM (chunksOf 3) (getElems b)

-- | Generates a string version of the board state
-- | from the Board directly.  This basically just glues
-- | 'showBoard' and 'showSquares' together.
showBoard :: Board -> IO String
showBoard b = liftM showSquares (boardToSquares b)

-- | Converts a list of list of squares representing a board state
-- | into a printable string for output.
showSquares :: [[Square]] -> String
showSquares = unlines . addBars . (map . map) showSquare . transpose
    where addBars = intersperse "-----" . map (intersperse '|')

-- | Converts a game square into the character used to represent it.
showSquare :: Square -> Char
showSquare = head . maybe " " show

-- | Evaluates a board and gives the current status of the game based upon it.
gameState :: [[Square]] -> GameState
gameState board
    | draw board          = Draw
    | won board == Just X = Won X
    | won board == Just O = Won O
    | otherwise           = InProgress
  where
    draw      = notElem Nothing . concat
    won board = case find full $ possibleRows board of
        Just xs -> head xs
        Nothing -> Nothing
      where
        full [x,y,z] = x == y && y == z && isJust x
        possibleRows board = board ++ transpose board ++ diagonals board
        diagonals [[x1, _,x3]
                  ,[ _,y2, _]
                  ,[z1, _,z3]] = [[x1,y2,z3],[x3,y2,z1]]

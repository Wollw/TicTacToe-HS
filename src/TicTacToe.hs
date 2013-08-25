module TicTacToe where

import Control.Monad (liftM)

import Data.Array.IO (IOArray)
import Data.Array.MArray (newListArray, readArray, writeArray, getElems)
import Data.List (find, intersperse, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

import qualified Graphics.UI.Fungen as FGE

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

isValidCoordinate :: Board -> (Int,Int) -> IO Bool
isValidCoordinate board (x,y) = 
    if (x,y) <= (2,2)
        then do spaceState <- readArray board (x,y)
                case spaceState of
                    Nothing -> return True
                    Just _  -> return False
        else return False

placePiece b p c = writeArray b c (Just p)

-- | Creates a new empty board to play on.
newEmptyBoard :: IO Board
newEmptyBoard = newListArray ((0,0), (2,2)) $ replicate 9 Nothing

-- | Creates a list of lists of Squares from a Board used for printing
-- | the board state to the screen.
boardToSquares :: Board -> IO [[Square]]
boardToSquares b = liftM (chunksOf 3) (getElems b)

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

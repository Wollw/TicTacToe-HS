module TicTacToe where

import Data.Array (Array, bounds, listArray, (//), elems, (!), ixmap)
import Data.List (transpose, find)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, fromJust)
import Data.Tuple (swap)

data GameState = InProgress { player :: Player
                            , board  :: Board
                            }
               | Won Player
               | Draw
               deriving (Show)

data Player = X | O deriving (Show, Eq, Enum)

-- | Replacement for default Enum's succ
--   allowing for Player values to wrap.
succWrap :: Player -> Player
succWrap O = X
succWrap p = succ p

-- | The representation of the squares of the TicTacToe board.
type Board = Array Position Square

type Square = Maybe Player

type Position = (Integer, Integer)

-- | A blank game state representing the initial
--   state of a game of TicTacToe.
newGame :: GameState
newGame = InProgress X emptyBoard

-- | A 3x3 array of Squares representing a board with
--   no pieces placed on it.
emptyBoard :: Board
emptyBoard = listArray ((1,1),(3,3)) $ replicate 9 Nothing

-- | This operator attempts to place a player's piece
--   on the board.
(/?/) :: GameState -> Position -> Maybe GameState
gs@(InProgress p b) /?/ position =
    if validIndex position
    then case b ! position of
                Nothing -> Just $ gs { board = b // [(position, Just p)] }
                _       -> Nothing
    else Nothing
  where
    validIndex (x,y) = x >= 1 && y >= 1 && x <= 3 && y <= 3
gs /?/ _ = Nothing

-- | Evaluates a GameState to determine what the next game state
--   should be.
nextGameState :: GameState -> GameState
nextGameState gs@(InProgress player board) = case nextGameState' board of
    gs@(InProgress player board) -> gs { player = succWrap player }
    gs -> gs
  where
    nextGameState' b = case find full $ rows board of
        Just xs -> Won . fromJust . head $ xs
        Nothing -> if notElem Nothing . elems $ board
                   then Draw
                   else gs
      where
        full [x,y,z] = x == y && y == z && isJust x
        toLists      = chunksOf 3 . elems
        rows b       = toLists b ++ (transpose . toLists) b ++ diagonals b
        diagonals b  = [ [b ! (1,1), b ! (2,2), b ! (3,3)]
                       , [b ! (3,1), b ! (2,2), b ! (1,3)] ]
nextGameState gs = gs

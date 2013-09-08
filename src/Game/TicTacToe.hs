{-# LANGUAGE Safe #-}
-- | This module contains functions and data used for building
--   a game of TicTacToe.
module Game.TicTacToe where

import Data.Array (Array, listArray, (//), elems, (!), indices)
import Data.List (transpose, find)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, fromJust)

-- | Representation of the current state of the game.
data GameState = InProgress { player :: Player
                            , board  :: Board
                            }
               | Won Player
               | Draw
               deriving Show

-- | Representation of each player in the game.
data Player = X | O deriving (Show, Eq, Enum)

-- | Replacement for default Enum's succ
--   allowing for Player values to wrap.
succWrap :: Player -> Player
succWrap O = X
succWrap p = succ p

-- | The representation of the squares of the TicTacToe board.
type Board = Array Position Square

-- | Representation of a square of the board.
--   A square either has Nothing on it or single
--   player's piece.
type Square = Maybe Player

-- | Coordinate type for board positions.
type Position = (Int, Int)

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
gs /?/ pos
    | invalidPosition = Nothing
    | inProgress gs   = Just $
        gs { board = board gs // [(pos, Just $ player gs)] }
    | otherwise = Nothing -- Game over or other unhandled states.
  where
    invalidPosition = (notElem pos . indices $ board gs)
                   || isJust (board gs ! pos)

-- | Evaluates a GameState to determine what the next game state
--   should be.
nextGameState :: GameState -> GameState
nextGameState gameState = case maybeFullRows gameState of
    Just xs -> Won . fromJust . head $ xs
    Nothing -> if boardFull gameState
                 then Draw
                 else gameState { player = succWrap . player $ gameState }
  where
    boardFull     = notElem Nothing . elems . board
    maybeFullRows = find full . rows . board
    toLists       = chunksOf 3 . elems
    full [x,y,z]  = x == y && y == z && isJust x
    full _        = False -- Fail if not three values
    rows b        = toLists b ++ (transpose . toLists) b ++ diagonals b
    diagonals b   = [ [b ! (1,1), b ! (2,2), b ! (3,3)]
                    , [b ! (3,1), b ! (2,2), b ! (1,3)] ]

-- | Determines if the game is currently in progress or not.
inProgress :: GameState -> Bool
inProgress (InProgress _ _) = True
inProgress _                = False

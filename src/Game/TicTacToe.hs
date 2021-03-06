{-# LANGUAGE Safe #-}
-- | This module contains functions and data used for building
--   a game of TicTacToe.
module Game.TicTacToe where

import Data.Array (Array, listArray, (//), elems, (!), indices)
import Data.List (transpose, find, nub)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, fromJust, isNothing)

-- | Representation of the current state of the game.
data GameState = GameState { player :: Player
                           , board  :: Board
                           , phase  :: GamePhase
                           } deriving Show

-- | Representation of the current phase the game is in.
data GamePhase = InProgress
               | Won Player
               | Draw
               deriving (Eq, Show)

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
newGame = GameState X emptyBoard InProgress

-- | A 3x3 array of Squares representing a board with
--   no pieces placed on it.
emptyBoard :: Board
emptyBoard = listArray ((1,1),(3,3)) . replicate 9 $ Nothing

-- | This operator attempts to place a player's piece
--   on the board and returns the updated game state
--   if it succeeds.
(/?/) :: GameState -> Position -> Maybe GameState
gs /?/ p
    | validPosition = Just . updateGamePhase $
                        gs { board  = board gs // [(p, Just $ player gs)]
                           , player = succWrap $ player gs
                           }
    | otherwise = Nothing
  where
    validPosition = inProgress gs                 -- game in progress
                 && (elem p . indices $ board gs) -- position is on board
                 && isNothing (board gs ! p)      -- square not taken

-- | Evaluates a GameState to determine what the next game state
--   should be based on its board state.
updateGamePhase :: GameState -> GameState
updateGamePhase gameState = case maybeFullRows gameState of
    Just xs -> gameState { phase = Won . fromJust . head $ xs }
    Nothing -> if boardFull gameState
               then gameState { phase = Draw }
               else gameState { phase = InProgress }
  where
    boardFull     = notElem Nothing . elems . board
    maybeFullRows = find full . rows . board
    toLists       = chunksOf 3 . elems
    full xs       = (length . nub) xs == 1 && (isJust . head) xs
    rows b        = toLists b ++ (transpose . toLists) b ++ diagonals b
    diagonals b   = [ [b ! (1,1), b ! (2,2), b ! (3,3)]
                    , [b ! (3,1), b ! (2,2), b ! (1,3)] ]

-- | Determines if the game is currently in progress or not.
inProgress :: GameState -> Bool
inProgress (GameState _ _ InProgress) = True
inProgress _                          = False

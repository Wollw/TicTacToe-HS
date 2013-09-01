module TicTacToe where

import Data.Array (Array, listArray, (//), elems, (!))
import Data.List (transpose, find)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, fromJust)

data GameState = InProgress { player :: Player
                            , board  :: Squares
                            }
               | Won Player | Draw deriving (Show)

data Player = X | O deriving (Show, Eq)

instance Enum Player where
    succ O   = X
    succ X   = O
    toEnum   = toEnum
    fromEnum = fromEnum

type Squares = Array Position Square
type Square = Maybe Player
type Position = (Integer, Integer)

-- | A blank game state representing the initial
--   state of a game of TicTacToe.
newGame :: GameState
newGame = InProgress X emptyBoard

-- | A 3x3 array of Squares representing a board with
--   no pieces placed on it.
emptyBoard :: Squares
emptyBoard = listArray ((0,0),(2,2)) $ replicate 9 Nothing

-- | This operator attempts to place a player's piece
--   on the board.
(/?/) :: Squares -> Position -> Player -> Maybe Squares
(/?/) squares position player = case squares ! position of
    Nothing -> Just $ squares // [(position, Just player)]
    _ -> Nothing

-- | Evaluates a GameState to determine what the next game state
--   should be.
nextGameState :: GameState -> GameState
nextGameState gs@(InProgress player board) = case nextGameState' board of
    gs@(InProgress player board) -> gs { player = succ player }
    gs -> gs
  where
    nextGameState' b = case find full $ rows board of
        Just xs -> Won . fromJust . head $ xs
        Nothing -> if notElem Nothing . concat $ boardList
            then Draw
            else gs
      where
        boardList = chunksOf 3 . elems $ board
        rows board = boardList ++ transpose boardList ++ diagonals boardList
        full [x,y,z] = x == y && y == z && isJust x
        diagonals [[x1, _,x3]
                  ,[ _,y2, _]
                  ,[z1, _,z3]] = [ [x1,y2,z3]
                                 , [x3,y2,z1]
                                 ]
nextGameState gs = gs

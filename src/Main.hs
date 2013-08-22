import Control.Monad (liftM)
import Data.Array.IO (IOArray)
import Data.Array.MArray (newListArray, readArray, writeArray, getElems)
import Data.List (find, intersperse, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

type Board  = IOArray (Int, Int) Square
type Square = Maybe Player
data Player  = X | O deriving (Show, Read, Eq)
data GameState = Won Player
               | Draw
               | InProgress
               deriving (Show, Read)

main = do
    putStr $  "TicTacToe!\n"
           ++ "----------\n"
           ++ "\n"
    newEmptyBoard >>= loop X

loop :: Player -> Board -> IO ()
loop player board = do
    putStrLn =<< showBoard board
    coordinate <- getCoordinate
    putStrLn ""

    placePiece board player coordinate
    squares <- boardToSquares board
    case gameState squares of
        Won p -> putStrLn $ "Player " ++ show p ++ " won!"
        Draw  -> putStrLn "The game resulted in a draw!"
        InProgress -> loop player' board
  where
    player' = if player == X then O else X
    placePiece b p c = writeArray b c (Just p)
    getCoordinate :: IO (Int, Int)
    getCoordinate = do
        putStr $ show player ++ ": "
        str <- getLine
        case readMaybe str of
            Just coord -> do
                spaceState <- readArray board coord
                case spaceState of
                    Nothing -> return coord
                    Just _  -> putStrLn "Square filled already." >> getCoordinate
            Nothing -> putStrLn "Invalid Coordinate." >> getCoordinate

newEmptyBoard :: IO Board
newEmptyBoard = newListArray ((0,0), (2,2)) $ replicate 9 Nothing

-- | Creates a list of lists of Squares from a Board used for printing
-- | the board state to the screen.
boardToSquares :: Board -> IO [[Square]]
boardToSquares b = liftM (chunksOf 3) (getElems b)

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

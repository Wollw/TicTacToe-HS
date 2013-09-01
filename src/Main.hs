import TicTacToe
import Data.Tuple (swap)
import Data.Array (elems)
import Data.List
import Data.List.Split
import Control.Monad
import Control.Monad.State

main :: IO ()
main = runGame newGame
runGame gs@(InProgress player board) = do
    putStrLn "=========="
    putStrLn . ppGameState $ gs
    putStrLn "Enter position:"
    pos <- fmap (swap . read) getLine
    case board /?/ pos $ player of
        Nothing -> putStrLn "Invalid position." >> runGame gs
        Just board' -> case nextGameState $ gs {board = board'} of
            Won p -> putStrLn $ "Player " ++ show p ++ " wins."
            Draw  -> putStrLn "Draw."
            gs    -> runGame gs

ppGameState :: GameState -> String
ppGameState (InProgress player board) =
        "Player: " ++ show player ++ "\n"
     ++ " Board:\n"
     ++ (intercalate "\n" . chunksOf 3 . concatMap ppSquare . elems $ board)

ppSquare :: Square -> String
ppSquare Nothing = " "
ppSquare (Just player)  = show player

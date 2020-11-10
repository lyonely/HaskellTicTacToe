module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver board@(cs , n)
    = checker (rows board ++ cols board ++ diags board)
    where
        checker (x : xs)
            = (nub x == [Taken O]) || (nub x == [Taken X]) || checker xs
        checker []
            = False

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition str
    = readMaybe str' :: Maybe Position
    where
        str' = "(" ++ [if (isSpace x) then ',' else x | x <- str] ++ ")"


tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player (x , y) (cs , n)
    | x < 0 || y < 0 || x >= n || y >= n    = Nothing
    | board_pos == Empty                    = Just (move cs pos player , n)
    | otherwise                             = Nothing
    where
        pos = x * n + y
        board_pos = cs !! pos
        move (c' : cs') pos player
            | pos == 0          = (Taken player : cs')
            | otherwise         = (c' : x')
            where x' = move cs' (pos - 1) player


-------------------------------------------------------------------
-- I/O Functions

prettyPrint :: Board -> IO ()
prettyPrint board@(cs , n)
    = putStr ((foldr1 (newline) (map (intersperse ' ') (map replacer (rows board))))
    ++ "\n")
    where
        newline :: String -> String -> String
        newline x y   = x ++ "\n" ++ y
        replacer :: [Cell] -> String
        replacer []             = ""
        replacer row@(x : xs)
            | x == Taken O      = 'O' : replacer xs
            | x == Taken X      = 'X' : replacer xs
            | x == Empty        = '-' : replacer xs

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn (cs , n) player
    = do
        if player == O
            then do
                let name = "O"
                putStr ("Player " ++ name ++ ", please enter your move (row col): ")
                place <- getLine
                let xy = parsePosition place
                if xy == Nothing
                    then do
                        putStr("Wrong move.")
                        takeTurn (cs, n) player
                    else do
                        let move = tryMove player (fromJust xy) (cs , n)
                        if move == Nothing
                            then takeTurn (cs , n) player
                            else do
                                    return (fromJust move)
            else do
                let name = "X"
                putStr ("Player " ++ name ++ ", please enter your move (row col): ")
                place <- getLine
                let xy = parsePosition place
                if xy == Nothing
                    then do
                        putStr("Wrong move.")
                        takeTurn (cs, n) player
                    else do
                        let move = tryMove player (fromJust xy) (cs , n)
                        if move == Nothing
                            then takeTurn (cs , n) player
                            else do
                                    return (fromJust move)


-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board player
    = do
        prettyPrint board
        newboard <- takeTurn board player
        if gameOver newboard
            then if player == O
                    then do
                        let name = "O"
                        putStr ("Congratulations Player " ++ name ++ "!\nThank you for playing\n")
                    else do
                        let name = "X"
                        putStr ("Congratulations Player " ++ name ++ "!\nThank you for playing\n")
            else if player == O
                    then playGame newboard X
                    else playGame newboard O


-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.

main :: IO ()
main
    = do
        putStr "Welcome to Tic Tac Toe.\nPlease choose a board size: "
        num <- getLine
        let dimension = readMaybe num :: Maybe Int
        if dimension == Nothing
            then do
                    dimension <- dimension_get
                    let board = boardmaker dimension
                    putStr "Player O goes first.\n"
                    playGame board O
            else do
                    let board = boardmaker dimension
                    putStr "Player O goes first.\n"
                    playGame board O

boardmaker :: Maybe Int -> Board
boardmaker number
    = ([Empty | x <- [1,2..num^2]], num)
    where num = fromJust number


dimension_get
    = do
        putStr "Invalid input, please try again: "
        num <- getLine
        let dimension = readMaybe num :: Maybe Int
        if dimension == Nothing
            then dimension_get
            else return (dimension)


-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

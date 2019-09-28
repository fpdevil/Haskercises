{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise21.hs
Description: General examples and exercises in Haskell
             Some tasks from Programming in Haskell - G Hutton
-}

module Haskercise21 where

import           Control.Applicative
import           Control.Monad
import qualified Data.Char           as C
import           System.IO
-----------------------------------------------------------------------------------
-- | Hangman game
hangman :: IO ()
hangman = do putStrLn "Think of a Word."
             word <- sgetLine
             putStrLn "Guess the word..."
             playWith word

-- | read a string of characters from the keyboard and echo
--   each character as a dash `-` to keep the secret
sgetLine :: IO String
sgetLine = do x <- getSingleChar
              if x == '\n'
                 then do putChar x
                         return []
                 else do putChar '-'
                         xs <- sgetLine
                         return (x : xs)

-- | get a single character from the keyboard without echoing
--   first turn the echo off and then later turn on with hSetEcho
getSingleChar :: IO Char
getSingleChar = do hSetEcho stdin False
                   x <- getChar
                   hSetEcho stdin True
                   return x

playWith :: String -> IO ()
playWith word = do putStr "? "
                   guess <- getLine
                   if guess == word
                   then putStrLn "Right guess!"
                   else do putStrLn (match word guess)
                           playWith word

-- | matching 2 strings char by char
match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]
-----------------------------------------------------------------------------------
-- | Nim
--   a board game consisting of 5 numbered rows of stars initially as
--
--   1: * * * * *
--   2: * * * *
--   3: * * *
--   4: * *
--   5: *


-- | Two players start by removing the stars from the end on each line
--   the player who empties the board first wins the game
--   a function to get the next player
nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1

-- | represent the board as a list of number of stars on each row
type Board = [Int]

-- | initial board state contains number of stars on each row
initial :: Board
initial = [5,4 .. 1]

-- | final or finished board state; returns true if all stars in
--   each row is zero or all list values are zeroes
finished :: Board -> Bool
finished = all (== 0)

-- | a valid move is the one specified by the row number and number of
--   stars to be removed; a star can be removed from a row only if it
--   has sufficient number of stars which are at-least equal to num
valid :: Board -> Int -> Int -> Bool
valid brd row num = snd (z !! (row-1)) >= num
  where
    z :: [(Int, Int)]
    z = zip [1 ..] brd

-- | given a board, row, number remove the number of stars
--   from the specified row only if it has enough stars
move :: Board -> Int -> Int -> Board
move brd row num = map (\(x, y) -> update (x, y) row num) (zip [1 ..] brd)
  where
    update (a, b) r n = if a == r && b >= n then b-n else b

-- λ> move initial 3 4
-- [5,4,3,2,1]
-- λ> move initial 3 1
-- [5,4,2,2,1]
-- λ> move initial 2 3
-- [5,1,3,2,1]

-- | given a row number and number of stars remaining, display the row
displayRow :: Int -> Int -> IO ()
displayRow row num = putStrLn . concat $ show row : ": " : replicate num "* "

-- λ> displayRow 2 3
-- 2: * * *

-- | display the board consisting of 5 rows
putBoard :: Board -> IO ()
putBoard board = sequence_ [displayRow x y | (x, y) <- zip [1 .. ] board]

-- λ> putBoard initial
-- 1: * * * * *
-- 2: * * * *
-- 3: * * *
-- 4: * *
-- 5: *

-- | helper function for printing newline
newline :: IO ()
newline = putChar '\n'

-- | helper function to display prompt and read a
--   user entered single char
getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     c <- getChar
                     newline
                     if C.isDigit c
                        then return (C.digitToInt c)
                        else do putStrLn "ERROR: Invalid digit"
                                getDigit prompt

-- | play the game taking current board and player
play :: Board -> Int -> IO ()
play board player = do newline
                       putBoard board
                       if finished board
                          then do newline
                                  putStr "Player "
                                  putStr $ show (nextPlayer player)
                                  putStrLn " wins!!!"
                          else do newline
                                  putStr "Player "
                                  print player
                                  -- get the row and star numbers
                                  row <- getDigit "Enter a Row number: "
                                  num <- getDigit "Enter Stars to remove: "
                                  if valid board row num
                                     then play (move board row num) (nextPlayer player)
                                     else do newline
                                             putStrLn "ERROR: Invalid move"
                                             play board player

-- start the NIM game
nim :: IO ()
nim = play initial 1

-----------------------------------------------------------------------------------
-- | Game of Life
--   It consists  of a  2 dimensional  board with cells  and each  cell can
--   either be  empty or filled  with a  single living cell.  Each internal
--   square has 8 immediate neighbors in any 8 directions around it. Now,
--   given an initial configuration, the next generation board is obtained
--   by simultaneously applying the below rules
--
--   a living cell survives if it has precisely 2 or 3 neighboring squares
--   which contain living cells
--
--   an empty  square gives birth  to a living cell  if it has  precisely 3
--   neighbors which contain living cells and remain empty otherwise


-- | Screem utilities
--   action for clearing the screen by displaying appropriate control char
clrscr :: IO ()
clrscr = putStr "\ESC[2J"

-- | Position of character
--   represent the position of each character on screen as (x, y)
--   the top left position is (1, 1)
type Pos = (Int, Int)

-- | Function to display a string at a given position by using the
--   control characters to move cursor to this position
writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- | board height and width
width :: Int
width = 10

height :: Int
height = 10

-- | Represent the board as a list of (x, y) positions at which
--   there is a living cell
type LifeBoard = [Pos]

-- | an example board can be represented as below
glider :: LifeBoard
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- | rendering functions for displaying the living cells
showCells :: LifeBoard -> IO ()
showCells lb = sequence_ [writeat p "O" | p <- lb]

-- | check if a cell is living or dead
isAlive :: LifeBoard -> Pos -> Bool
isAlive lb p = p `elem` lb

-- | check if a cell is dead or the position on board is empty
isDead :: LifeBoard -> Pos -> Bool
isDead lb p = not (isAlive lb p)

-- | a function for getting the neighbors of a cell in 8 directions
neighbors :: Pos -> [Pos]
neighbors (x, y) = map auxWrap [(x-1, y-1), (x, y-1), (x+1, y-1), (x+1, y)
                               ,(x-1, y+1), (x, y+1), (x+1, y+1)]

auxWrap :: Pos -> Pos
auxWrap (x, y) = ((x-1 `mod` width) + 1, (y-1 `mod` height) + 1)

-- | calculate the number of live neighbors for a given position, by
--   producing the list of its neighbors, retaining those which are
--   all alive and counting their number
liveNeighbors :: LifeBoard -> Pos -> Int
liveNeighbors = undefined


-----------------------------------------------------------------------------------
-- Twitter's Waterfall problem
-- We  are given  a  list  of numbers,  representing  walls of  different
-- height.  The  goal  is  to  determine  how  much  rain  water  can  be
-- accumulated between the walls. width of wall assumed as unit (1)
-- example [2, 5, 1, 2, 3, 4, 7, 7, 6]
--
--  1|
--  2|              ___
--  3|             |7 7|_
--  4|    _        |    6|
--  5|   |5|      _|     |
--  6|   | |    _|4      |
--  7|  _| |  _|3        |
--  8| |2  |_|2          |
--  9|_|____1____________|___
-- 10|  0 1 2 3 4 5 6 7 8 9
--
-- Each wall can hold water upto the level of smallest of the wall height
-- between  wall on  left and  wall on  right. In  each pass  the maximum
-- height of wall towrds right and  towards left has to be calculated and
-- the mimimum  of the two is  chosen. The sum of  absolute difference of
-- each of  these values  with the  wall height at  each point  gives the
-- water level above the wall
water :: [Int] -> Int
water xs = sum . getZipList $ (\a b -> abs (a-b)) <$> ZipList xs <*> ys
  where
    ys = min <$> ZipList (scanl1 max xs) <*> ZipList (scanr1 max xs)

-- λ> water [2, 5, 1, 2, 3, 4, 7, 7, 6]
-- 10

-----------------------------------------------------------------------------------
-- | a little logic puzzle Oh hi
--   http://0hh1.com/
--   The rules of the game are as follows
--
--   Fill a square containing cells with Red and Blue tiles
--   Never have 3 Red or 3 Blue tiles together in a Row
--   Never have 3 Red or 3 Blue tiles together in a Column
--   A full Row must have same number of Red and Blue tiles
--   No 2 Rows or Columns can be same


-- | Represent the Data model of the Puzzle
data Cell = Empty
          | Red
          | Blue

type Row = [Cell]
type Puzzle = [Row]

instance Show Cell where
  show Empty = "E"
  show Red   = "R"
  show Blue  = "B"

instance Eq Cell where
  Red == Red = True
  Blue == Blue = True
  _ == _ = False

-- | Since no cell needs to be empty check for that
isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Check if any two or more rows are the same
--   if response is False the rows pass check
hasDuplicateRows :: (Eq a) => [[a]] -> Bool
hasDuplicateRows []       = False
hasDuplicateRows (r : rs) = r `elem` rs || hasDuplicateRows rs

-- | No 3 consecutive cells can be of the same color (Red | Blue)
--   if the response is True the rows fail check
hasThreeSet :: Row -> Bool
hasThreeSet row = any (\rs -> all (== Red) rs || all (== Blue) rs) (groupN 3 row)

-- | Take and group each N elements from a List
groupN :: Int -> [a] -> [[a]]
groupN _ []       = []
groupN n z@(x:xs) = take n z : groupN n xs

-- | Check if there are equal number of colors in each row
hasEqualColors :: Row -> Bool
hasEqualColors row = any isEmpty row || reds == blues
                     where
                       reds = length $ filter (== Red) row
                       blues = length $ filter (== Blue) row

-- | Check if a single Row is valid based on the above predicates
isRowValid :: Row -> Bool
isRowValid row = hasThreeSet row && hasEqualColors row

-- | Check if all the Rows of the puzzle are valid
areRowsValid :: [Row] -> Bool
areRowsValid rows = all isRowValid rows && not (hasDuplicateRows rows)

-- | Transpose of a list of lists
transpose :: [[a]] -> [[a]]
transpose []               = []
transpose ([] : xss)       = transpose xss
transpose ((x : xs) : xss) = (x : [h | (h : _) <- xss]) : transpose (xs : [t | (_ : t) <- xss])

-- | Check if the puzzle as a whole is valid
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle puzzle = areRowsValid (transpose puzzle)

-- | Get all the possible combination(s) of filling each row,
--   containing the Empty cells. This contains all the possible
--   combinations, so a further filtering is necessary for
--   picking the valid rows as defined next
rowCombinations :: Row -> [Row]
rowCombinations [] = [[]]
rowCombinations (x : xs)
  | isEmpty x = map (Red :) ys ++ map (Blue :) ys
  | otherwise = map (x :) ys
  where
    ys = rowCombinations xs

-- | Get all possible valid row combinations
rowPossibilities :: Row -> [Row]
rowPossibilities row = filter isRowValid (rowCombinations row)

-- | Get all the solutions for a given puzzle
getSolutions :: Puzzle -> [Puzzle]
getSolutions [] = [[]]
getSolutions (x : xs) = do
  y <- rowPossibilities x
  guard (isValidPuzzle $ y : xs)
  ys <- getSolutions (y : xs)
  return (y : ys)


-- a 4 X 4 puzzle
test :: Puzzle
test = [[Empty, Empty, Red, Empty]
       ,[Empty, Empty, Empty, Blue]
       ,[Blue, Empty, Empty, Empty]
       ,[Red, Red, Empty, Empty]]

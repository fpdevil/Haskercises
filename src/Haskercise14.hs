{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise14.hs
Description: General exercises in Haskell.
             N-Queens
-}

module Haskercise14
(
bitTriangle,
isDiagonalTo,
isSafeOn,
lineBuilder,
nqueensSlow,
nqueens,
main
)
where

import           Control.Monad
import qualified Data.List          as L
import           System.Environment
-----------------------------------------------------------------------------------
-- a triangle using bits 0 and 1
bitTriangle :: Int -> IO ()
bitTriangle n = putStrLn (triangle n)
                where
                  triangle x = L.intercalate "\n" $ map f [1 .. x]
                  f i = unwords $ map show $ take i $ map (`rem` 2) [i ..]

-- λ> bitTriangle 5
-- 1
-- 0 1
-- 1 0 1
-- 0 1 0 1
-- 1 0 1 0 1
-----------------------------------------------------------------------------------
{-
N-Queens Problem

The N-Queens problem  consists of placing N Queens on  a chessboard so
that none of them  attack each other; which means no  2 queens must be
on the same row, same column or same diagonal

let i and  j be row and column;  iq and jq are the queens  on i-th row
and j-th column respectively; we must  ensure that they do not fall on
the same row or same column

iq and jq are on same column if iq == jq
iq and jq are on same diagonal if abs(iq - jq) = abs(i -j)
-}
-- helper function for permutations
perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = [y : ys | y <- xs, ys <- perms (L.delete y xs)]

-- very inefficient because of the usage of permutations
-- resulting init O(n!) time
nqueensSlow :: Int -> [[Int]]
nqueensSlow n = [qs | qs <- perms [1 .. n], dontAttack qs]
                  where
                    dontAttack :: [Int] -> Bool
                    dontAttack qs = and [abs ((qs !! i) - (qs !! j)) /= abs (i - j) |
                                         i <- [0 .. n - 2],
                                         j <- [i + 1 .. n - 1]]

-- a better solution will be to check that does not attack another queen as
-- soon as it is placed on the  chess board instaed of checking the whole
-- board

-- define the position of a queen as type Queen at (x, y) location
type Queen = (Int, Int)
-- define the Board filled with Queens; this woulb be the result
type Board = [Queen]

{-
define  a   helper  function  solver   for  placing  n  queens   in  m
positions.   We  will   solve  this   recursively  using   guards  for
backtracking; a single queen n can  be placed on any of the positional
columns 1 through n. This should  not interfere with the n-1 queens to
be placed;  at any point if  it clashes then guard  will backtrack the
problem to a previous state and continue with a fresh iteration.
-}
solver :: Int -> [Board]
solver n = placer n n

-- initially  the number of columns or positions is the same as number
-- of queens; the helper function here starts with that; the base case
-- would be if queens are 0 then the obvious result would be [[]] which
-- would be also the terminating condition for the problem.
placer :: Int -> Int -> [Board]
placer 0 _ = [[]]
placer n m = do
  queensAcc <- placer (n - 1) m           -- placing n-1 queens on m cols
  column <- [1 .. m]                      -- pick a column from 1 to m
  let queen = (n, column)                -- place n th queen on column
  guard $ queen `isSafeOn` queensAcc     -- check if queen is safe with others
  return (queen : queensAcc)

-- check if one queen is placed diagonnally with another queen
isDiagonalTo :: Queen -> Queen -> Bool
isDiagonalTo (x1, y1) (x2, y2) = abs (x1 - x2) == abs (y1 - y2)

-- check if one queen can attack another queen
-- queens will attack if they are on the same row, col or diagonal
willAttack :: Queen -> Queen -> Bool
willAttack q1@(x1, y1) q2@(x2, y2) = x1 == x2 ||
                                     y1 == y2 ||
                                     q1 `isDiagonalTo` q2

-- two queens will be safe if they do not attack each other
-- run that condition on a board of queens given a queen
isSafeOn :: Queen -> Board -> Bool
isSafeOn q = not . any (willAttack q)

nqueens :: Int -> [Board]
nqueens = solver

-- helper function for building a string with Queen position
lineBuilder :: Int -> Int ->[String]
lineBuilder n m = replicate (m - 1) "* " ++ ["Q"] ++ replicate (n - m) " *"

-- rendering the results with the Main
main :: IO ()
main = do
  [n] <- getArgs
  let m = read n :: Int
  let qs = nqueens m
  sequence_ [mapM_ print q |
             q <- [("For Solution set: " ++ show z) : [concat (lineBuilder m y) |
             (_, y) <- z] |
             z <- qs]]

{-
Test run with 4 Queens

λ> :main 4
"For Solution set: [(4,3),(3,1),(2,4),(1,2)]"
"* * Q *"
"Q * * *"
"* * * Q"
"* Q * *"
"For Solution set: [(4,2),(3,4),(2,1),(1,3)]"
"* Q * *"
"* * * Q"
"Q * * *"
"* * Q *"
-}

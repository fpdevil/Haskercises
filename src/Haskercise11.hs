{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise11.hs
Description: Logic Puzzle solving with Haskell
             Solving Mr. S and Mr. P of McCarthy
-}

module Haskercise11
(
numbersRange,
factorTable,
summandTable,
validFactors,
validSummands,
singleton,
fact1,
fact2,
fact3,
fact4,
fact5,
main
)
where

import           Control.Monad

-----------------------------------------------------------------------------------
{-
Solving  the "Mr.S  and Mr.P"  puzzle by  John McCarthy.  We pick  two
numbers a  and b, so  that a>=b and both  numbers are within  the range
[2,99]. We give Mr.P the product a*b and give Mr.S the sum a+b

The following dialog takes place:

	Mr.P: I don't know the numbers
	Mr.S: I knew you didn't know. I don't know either.
	Mr.P: Now I know the numbers
	Mr.S: Now I know them too

Can we find the numbers a and b?
-}

-- define the range of numbers required for the problem space (2 to 99
numbersRange :: [Int]
numbersRange = [2 .. 99]

{-
condition for P; given  a number p, find all the factors  a and b such
that a ≤ b and a • b ≈ p
-}
factorTable :: [[(Int, Int)]]
factorTable = map f [0 ..]
                where
                  f p = [(a, b) | a <- numbersRange, b <- numbersRange, a >= b, a * b == p]

-- test run
-- λ> factorTable !! 12
-- [(4,3),(6,2)]
-- λ> factorTable !! 15
-- [(5,3)]

-- get all the valid factor pairs for a given number p
validFactors :: Int -> [(Int, Int)]
validFactors p = factorTable !! p

{-
condition for S; given a number s,  find all the summands a and b such
that (a + b) ≈ p and a ≥ b
-}
summandTable :: [[(Int, Int)]]
summandTable = map f [0 ..]
                where
                  f s = [(a, b) | a <- numbersRange, b <- numbersRange, a >= b, a + b == s]

-- Test run
-- λ> summandTable !! 12
-- [(6,6),(7,5),(8,4),(9,3),(10,2)]
-- λ> summandTable !! 15
-- [(8,7),(9,6),(10,5),(11,4),(12,3),(13,2)]


-- get all the valid summand pairs for a given number s
validSummands :: Int -> [(Int, Int)]
validSummands s = summandTable !! s


-- test if a given list is a Singleton; containing only one element
singleton :: [a] -> Bool
singleton [_] = True
singleton _   = False

{-
encoding the fact 1
Mr.P doesn't know the numbers; the condition for P to know the numbers
is the product of a and b as in (a * b) = p; so obiviously P will know
the number if  it has valid factors and  the product of a and  b has a
good unique factorization
-}
fact1 :: (Int, Int) -> Bool
fact1 (a, b) = (not . singleton . validFactors) (a * b)

{-
encoding the fact 2
S  does  not  know  the  numbersMr.S doesn't  know  the  numbers;  the
condition for S to know the numbers is the sum of a and b as init (a +
b) =  s; so  obviously S  will know  the number  if iterate  has valid
summands a and b such that the sum of a and b has a unique value
-}
fact2 :: (Int, Int) -> Bool
fact2 (a, b) = (not . singleton . validSummands) (a + b)

{-
encoding fact 3
Mr.S knows that Mr.P doesn't know the numbers; S can be sure that P is
not aware  of the  numbers if for  all the possible  summands a  and b
making the expression (a + b), P cannot be certain of the factors
-}
fact3 :: (Int, Int) -> Bool
fact3 (a, b) = all fact1 $ validSummands (a + b)

{-
encoding fact 4
Mr.P: Now I know the numbers; in other words, Mr.P now knows that fact
3 is true and he is able to  find the numbers; this can happen only if
for all  the factors a  and b  forming (a *  b) there exists  only one
combination making the fact 3 true
-}
fact4 :: (Int, Int) -> Bool
fact4 (a, b) = singleton . filter fact3 $ validFactors (a * b)

{-
encoding the fact 5 (final)
Mr.S: Now I know them too; in other words, Mr.S. knows that Mr.P found
the numbers; this can happen only if  for all the possible values of a
and b forming  (a + b), there exists only  one combination which makes
the fact 4 true.
-}
fact5 :: (Int, Int) -> Bool
fact5 (a, b) = singleton . filter fact4 $ validSummands (a + b)

-- result calculation finally we calculate the list of all the numbers
-- matching the  above facts from 1  through 5 to arrive  at the final
-- result which are the numbers of S and P
main :: [(Int,Int)]
main = do
  a <- numbersRange
  b <- numbersRange
  guard (a >= b)
  guard $ all ($ (a, b)) [fact1, fact2, fact3, fact4, fact5]
  return (a, b)

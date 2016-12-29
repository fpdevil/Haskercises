{-|

   Author     : Sampath Singamsetty
   Maintainer :
   File       : Haskercise10.hs
   Description: Logic Puzzle solving with Haskell (from SICP)
                The example is from the non determinism section
-}

module Haskercise10
(
isAdjacent,
isDistinct,
whereAreAll,
main
)
where

import           Control.Monad
-----------------------------------------------------------------------------------
{-
SICP (4.3.3) Non Determinism
Baker, Cooper, Fletcher, Miller, and Smith live on different floors of
an apartment house that contains only five floors. Baker does not live
on the top  floor. Cooper does not live on  the bottom floor. Fletcher
does not live on either the top or the bottom floor. Miller lives on a
higher floor than does Cooper. Smith does not live on a floor adjacent
to  Fletcher’s.  Fletcher  does  not  live  on  a  floor  adjacent  to
Cooper’s. Where does everyone live?
-}

-- declare type system for Name of Person and his Floor number
type Name = String
type Floor = Int

-- define a data type Positions for describing the Person and the floor
-- number in which he stays; declared as a record
data Positions = Positions { person :: Name
                           , floorN :: Floor
                           }

-- make the Positions data type an instance of Show which will be later
-- useful for rendering purposes in the main method.
instance Show Positions where
  show p = "Mr. " ++ show (person p) ++ " lives on floor no. " ++ show (floorN p)

-- check if two floors are adjacent to each other
-- 2 floors are adjacent if their numerical difference = 1
isAdjacent :: Floor -> Floor -> Bool
isAdjacent floorX floorY = abs (floorX - floorY) == 1

-- check if the elements of a list are all distinct or unique
isDistinct :: (Ord a) => [a] -> Bool
isDistinct []       = True
isDistinct [_]      = True
isDistinct (x : xs) = (x `notElem` xs) && isDistinct xs

-- function for checking who lives on which floor based on the conditions
-- provided in the problem specification; guard function from the package
-- Control.Monad is used to consider the non determinism and automatic
-- backtracking calculations to arrive at a valid solution
-- List monad is used to hold the floor numbers on which each person stays
whereAreAll :: [[Positions]]
whereAreAll = do
  baker    <- [1 .. 5]
  cooper   <- [1 .. 5]
  fletcher <- [1 .. 5]
  miller   <- [1 .. 5]
  smith    <- [1 .. 5]
  guard $ isDistinct [baker, cooper, fletcher, miller, smith]
  guard (baker /= 5)
  guard (cooper /= 1)
  guard (fletcher /= 1 && fletcher /= 5)
  guard (miller > cooper)
  guard $ not (isAdjacent smith fletcher)
  guard $ not (isAdjacent fletcher cooper)
  return [Positions { person = x, floorN = y} | (x, y) <- zip ["Baker", "Cooper", "Fletcher", "Miller", "Smith"]
                                                             [baker, cooper, fletcher, miller, smith]]

-- main method for running the program
main :: IO ()
main = mapM_ print $ concat whereAreAll

-----------------------------------------------------------------------------------
-- solutions by running the code
-- λ> main
-- Mr. "Baker" lives on floor no. 3
-- Mr. "Cooper" lives on floor no. 2
-- Mr. "Fletcher" lives on floor no. 4
-- Mr. "Miller" lives on floor no. 5
-- Mr. "Smith" lives on floor no. 1

{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise13.hs
Description: General exercises in Haskell. Includes a Logic Puzzle from the
             classic River crossing puzzles section.
-}
module Haskercise13
(
)
where

import           Control.Applicative
import           Control.Monad
-----------------------------------------------------------------------------------
-- define an Identity data type
-- data Identity a = Identity a deriving (Show)
-- redefining using newtype
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    -- fmap f (Identity x) = Identity (f x)
    fmap f = Identity . f . runIdentity

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return = Identity
    x >>= f = f (runIdentity x)

-----------------------------------------------------------------------------------
{-
A River crossing puzzle - Goat, Cabbage and Wolf (Moscow Puzzles)

A man has to take a wolf, a goat, and some cabbage across a river. His rowboat
has enough room for the man plus either the wolf or the goat or the cabbage. If
he takes the cabbage with him, the wolf will eat the goat. If he takes the wolf,
the goat will eat the cabbage. Only when the man is present are the goat and the
cabbage safe from their enemies. All the same, the man carries wolf, goat, and
cabbage across the river. How?

Solution:

The wolf does not eat cabbage, so the crossing can start with the goat.

The man leaves the goat and returns, puts the cabbage in the boat and takes it
across. On the other bank, he leaves the cabbage but takes the goat.

He leaves the goat on the first bank and takes the wolf across. He leaves the
cabbage with the wolf and rows back alone.

He takes the goat across.
-}

-- first define data types for holding each item(s) Farmer, Goat, Wolf, Cabbage
data Item = Farmer
          | Goat
          | Wolf
          | Cabbage
          deriving (Show, Eq, Enum)

-- for transferring or crossing each item across the island
newtype Transfer = Cross Item deriving (Eq)

-- make Transfer an instance of the Show
instance Show Transfer where
    show (Cross Farmer)  = "F"
    show (Cross Goat)    = "G"
    show (Cross Wolf)    = "W"
    show (Cross Cabbage) = "C"

-- here the solution consists of a series of decision(s) which in turn consists
-- of a series of transfers; a Decision can be made to cross item in terms of a
-- series of Transfer(s) where a Transfer could be any of the following options
-- Cross the Farmer
-- Cross the Wolf
-- Cross the Goat
-- Cross the Cabbage
--
type Decision = [Transfer]

-- define the current position of each item wether it is on Island or Shore
data Position = Shore
              | Island
                deriving (Show, Eq)

-- initially nothing has been transferred yet and everything is on the Island
-- which means that the list of Transfers is empty or no decision made yet
initialState :: Decision
initialState = []

{-
Position of an Item after a series of Transfers/Crosses

get the position of an item after a series of Transfers or after a Decision has
been made; item could be on the Island (0 or even crosses) or on the Shore (1 or
odd crosses); the final position of the Farmer depends on the odd or even number
of crosses he has made as with each successive Transfer or a Cross, the position
of the Farmer is changed

     Island          =>         Shore
================            ===============
Cross = 0 (even)            Cross = 1 (Odd)
-}
getPosition :: Item -> Decision -> Position
getPosition i d = case i of
                    Farmer -> positionFromStart . length $ d
                    i      -> positionFromStart . length $ filter (== Cross i) d
                    where
                        positionFromStart :: Int -> Position
                        positionFromStart n
                          | even n    = Island
                          | otherwise = Shore

--λ> :t getPosition Wolf $ Cross <$> [Goat,Farmer,Wolf,Goat]
--getPosition Wolf $ Cross <$> [Goat,Farmer,Wolf,Goat] :: Position
--λ> getPosition Wolf $ Cross <$> [Goat,Farmer,Wolf,Goat]
--Shore
--λ> getPosition Farmer $ Cross <$> [Goat,Farmer,Wolf,Goat]
--Island

-- check that the position(s) returned after decisions are valid solutions
-- the decisions are valid solutions if all the items are on Shore
isValid :: Decision -> Bool
isValid d = getPosition Farmer d == Shore
         && getPosition Goat d    == Shore
         && getPosition Wolf d    == Shore
         && getPosition Cabbage d == Shore

-- another way for the same solution using map
isSolution :: Decision -> Bool
isSolution d = all (== Shore) positionList
    where
        positionList = map (`getPosition` d) [Farmer .. ]

-- in any Transfer or Decision, the Farmer always needs to be besides the Item.
-- so, a Transfer is valid if Farmer is besides the Item
isValidTransfer :: Decision -> Transfer -> Bool
isValidTransfer d (Cross Farmer) = True
isValidTransfer d (Cross i)      = getPosition i d == getPosition Farmer d


-- avoiding the redundant Transfers; to avoid the cases where a Farner or a
-- goat is moved more than once in a row.
avoidRedundancy :: Decision -> Transfer -> Bool
avoidRedundancy [] _ = False
avoidRedundancy d t  = last d == t


-- apart from the Transfer Decision being valid, it has to be a safe one
-- a Transfer is safe if nothing eats the other
isTransferSafe :: Decision -> Bool
isTransferSafe d = goatsPosition == farmersPosition
                || safeGoatsPosition && safeCabbagesPosition
                where
                    goatsPosition        = getPosition Goat d
                    farmersPosition      = getPosition Farmer d
                    safeGoatsPosition    = goatsPosition /= getPosition Wolf d
                    safeCabbagesPosition = getPosition Cabbage d /= goatsPosition


{-
function for resulting all valid crossings

define a function which when provided with a Decision, will return all the
successful ways of adding the Transfers to that Decision;
-}
allDecisions :: Decision -> [Decision]
allDecisions d = do
    crossings <- Cross <$> [Farmer ..]
    guard (isValidTransfer d crossings)
    guard $ not (avoidRedundancy d crossings)
    let furtherDecision = d ++ [crossings]
    guard (isTransferSafe furtherDecision)
    return furtherDecision


-- repeatedly cross n times
crossNTimes :: Int -> [Decision]
crossNTimes n = iterate (>>= allDecisions) (return initialState) !! n


-- get all the solutions; the function returns all the valid Decisions after
-- n Transfers are made between Island and Shore
getSoutions :: Int -> [Decision]
getSoutions n = do
    d <- crossNTimes n
    guard (isSolution d)
    return d


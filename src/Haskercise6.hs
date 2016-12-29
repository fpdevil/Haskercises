{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise6.hs
Description: General Monad examples and exercises in Haskell
-}

module Haskercise6 where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Monoid
import           Prelude
-----------------------------------------------------------------------------------
-- example for  parsing data  into hex values,  decimal values  and words
-- containing only alpha-numeric characters

-- define a data type for parsing the 3 types
data Parsed = Digit Integer
            | Hex Integer
            | Word String
            deriving Show

-- if a character is added to parsed representation of hex digit
parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex i) c = if isHexDigit c
                             then return $ Hex ((i * 16) + toInteger (digitToInt c))
                             else mzero
parseHexDigit _ _       = mzero

-- if a character is added to parsed representation of decimal digit
parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit i) c = if isDigit c
                            then return $ Digit ((i * 10) + toInteger (digitToInt c))
                            else mzero
parseDigit _ _         = mzero

-- if a character is added to parsed representation of word
parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word str) c = if isAlpha c
                            then return $ Word (str ++ [c])
                            else mzero
parseWord _ _          = mzero

-- this function tries to parse a digit as a Hex, Decimal and Word value
-- result will be a list of possible parses
parse :: Parsed -> Char -> [Parsed]
parse p c = parseHexDigit p c `mplus`
            parseDigit p c `mplus`
            parseWord p c

-- now parse a string and return possible values
parseStr :: String -> [Parsed]
parseStr str = do
   acc <- return (Hex 0) `mplus` return (Digit 0) `mplus` return (Word "")
   foldM parse acc str

-- λ> parseStr "abcdef"
-- [Hex 11259375,Word "abcdef"]
-- λ> parseStr "12345"
-- [Hex 74565,Digit 12345]
-----------------------------------------------------------------------------------
-- data analysis using monoids
-- define data types for Min and Max
data Min a = Min a | MinEmpty deriving (Show)

data Max a = Max a | MaxEmpty deriving (Show)

newtype Count = Count Int deriving (Show)

instance (Ord a) => Monoid (Min a) where
  mempty                    = MinEmpty
  MinEmpty `mappend` m      = m
  m `mappend` MinEmpty      = m
  (Min x) `mappend` (Min y) = Min (if x < y then x else y)

instance (Ord a) => Monoid (Max a) where
  mempty                    = MaxEmpty
  MaxEmpty `mappend` m      = m
  m `mappend` MaxEmpty      = m
  (Max x) `mappend` (Max y) = Max (if x < y then y else x)

instance Monoid Count where
  mempty                        = Count 0
  (Count x) `mappend` (Count y) = Count (x + y)

-- helper functions for constructing values of Monoids
csum :: a -> Sum a
csum = Sum

cproduct :: a -> Product a
cproduct = Product

cmin :: a -> Min a
cmin = Min

cmax :: a -> Max a
cmax = Max

count :: a -> Count
count _ = Count 1

-- helper monoid function for tuples
a2 :: (a -> b) -> (a -> c) -> a -> (b, c)
a2 x y = (,) <$> x <*> y

a3 :: (a -> b) -> (a -> c) -> (a -> d) -> a -> (b, c, d)
a3 x y z = (,,) <$> x <*> y <*> z

-- arithmetic mean
newtype Mean a = Mean (Sum a, Count) deriving (Show)

instance (Num c) => Monoid (Mean c) where
  mempty = Mean mempty
  (Mean m1) `mappend` (Mean m2) = Mean (m1 `mappend` m2)

mean :: a -> Mean a
mean v = Mean (Sum v, Count 1)

--    λ> let xs = [45,23,78,10,11,1.5]
--    λ> foldMap mean xs
--    Mean (Sum {getSum = 168.5},Count 6)
--    λ> let (Mean (Sum x, Count y)) = foldMap mean xs in x / fromIntegral y
--    28.083333333333332

{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise1.hs
Description: General examples and exercises in Haskell

-}
module Haskercise1
  (
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits           hiding (xor)
import qualified Data.Char           as C
import           Prelude             hiding (filter, foldl, foldr)
import           System.Environment  (getArgs)

-----------------------------------------------------------------------------------
-- Data type modeling with playing cards
data Suit
  = Spades
  | Hearts
  | Clubs
  | Diamonds
  deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
  show Spades   = "♠"
  show Hearts   = "♥"
  show Clubs    = "♣"
  show Diamonds = "♦"

-- card colors can be either red or black
data Color
  = Black
  | Red
  deriving (Show)

--  function for color picking
colors :: Suit -> Color
colors suit =
  case suit of
    Spades   -> Black
    Hearts   -> Red
    Clubs    -> Black
    Diamonds -> Red

-- Rank of cards
-- 2, 3 .. 10, J, Q, K, Ace
data Rank
  = Rank Int
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord)

instance Show Rank where
  show (Rank n) = show n
  show Jack     = "J"
  show Queen    = "Q"
  show King     = "K"
  show Ace      = "A"

-- list all the card ranks
listRanks :: [Rank]
listRanks = [Rank x | x <- [2 .. 10]] ++ [Jack, Queen, King, Ace]

-- who beats whom
isHigher :: Rank -> Rank -> Bool
isHigher r1 r2 = r1 > r2

-- Model the Card
-- A Card can have both a Rank as well as a Suit
data Card = Card
  { rank :: Rank
  , suit :: Suit
  }

instance Show Card where
  show (Card r s) = show r ++ show s

getRank :: Card -> Rank
getRank (Card r _) = r

getSuit :: Card -> Suit
getSuit (Card _ s) = s

{-
λ> Card Ace Diamonds
A♦
λ> getRank $ Card {rank=Ace, suit=Diamonds}
A
λ> getSuit $ Card {rank=Ace, suit=Diamonds}
♦
-}
-- Check if one card beats another
willBeat :: Card -> Card -> Bool
willBeat (Card r1 s1) (Card r2 s2) = s1 == s2 && isHigher r1 r2

-- Modelling a Hand of Cards
-- A Hand can be empty without cards or
-- A Hand can put one more card into an existing Card stack
data Hand
  = Empty
  | Put Card
        Hand

instance Show Hand where
  show Empty     = "_"
  show (Put c h) = show c ++ " " ++ show h

-- get a hand containing all cards of the same suit
samesuit :: Hand -> Suit -> Hand
samesuit Empty _ = Empty
samesuit (Put c h) s
  | getSuit c == s = Put c (samesuit h s)
  | otherwise = samesuit h s

-- Check if a hand beats a card
handBeat :: Hand -> Card -> Bool
handBeat Empty card     = False
handBeat (Put c h) card = willBeat c card || handBeat h card

-- Check if the hand contains a Card of the given suit
hasSuit :: Hand -> Suit -> Bool
hasSuit Empty s     = False
hasSuit (Put c h) s = getSuit c == s || hasSuit h s

-- Find the lowest card init the hand
lowestCard :: Hand -> Card
lowestCard Empty = error "Empty Hand"
lowestCard (Put c h)
  | getRank c <= rank lc = c
  | otherwise = lc
  where
    lc = lowestCard h

-- Get all the cards which beat the given card on hand
betterCards :: Hand -> Card -> Hand
betterCards Empty _ = Empty
betterCards (Put c h) card
  | c `willBeat` card = Put c (betterCards h card)
  | otherwise = betterCards h card

-----------------------------------------------------------------------------------
-- | remove an element circularly and rotate
remove :: [a] -> [[a]]
remove []     = [[]]
remove [_]    = [[]]
remove (x:xs) = xs : map (x :) (remove xs)

-- λ> mapM_ print $ remove [1..5]
-- [2,3,4,5]
-- [1,3,4,5]
-- [1,2,4,5]
-- [1,2,3,5]
-- [1,2,3,4]
-- permutations
-- how to insert an item non-deterministically, in any place, of a list
-- insert an element at start, or keep the head and insert in tail
-- the head after... non deterministic insert
ndinsert :: a -> [a] -> [[a]]
ndinsert x []       = [[x]]
ndinsert x z@(y:ys) = (x : z) : map (y :) (ndinsert x ys)

-- permutation
permutation :: [a] -> [[a]]
permutation = foldr (concatMap . ndinsert) [[]]

-- λ> mapM_ print $ permutation [1,2,3]
-- [1,2,3]
-- [2,1,3]
-- [2,3,1]
-- [1,3,2]
-- [3,1,2]
-- [3,2,1]
-- combinations of a given length
combs :: Int -> [a] -> [[a]]
combs 0 _      = [[]]
combs n (x:xs) = map (x :) (combs (n - 1) xs) ++ combs n xs
combs _ _      = []

-- λ> mapM_ print $ combs 3 [1,2,3,4,5]
-- [1,2,3]
-- [1,2,4]
-- [1,2,5]
-- [1,3,4]
-- [1,3,5]
-- [1,4,5]
-- [2,3,4]
-- [2,3,5]
-- [2,4,5]
-- [3,4,5]
-- selections from a list
selections :: [a] -> [(a, [a])]
selections []     = []
selections (x:xs) = (x, xs) : [(y, x : ys) | (y, ys) <- selections xs]

-- λ> selections "abc"
-- [('a',"bc"),('b',"ac"),('c',"ab")]
subsequences :: [a] -> [[a]]
subsequences []     = [[]]
subsequences (x:xs) = subsequences xs ++ map (x :) (subsequences xs)

-- λ> subsequences "abc"
-- ["","c","b","bc","a","ac","ab","abc"]
-- non-deterministic reasoning; partitions of an integer
-- split an integer n into k positive integers such that their sum is
-- equal to n;  the order of integers does not matter
partitions :: Int -> [[Int]]
partitions n = parts !! n

parts :: [[[Int]]]
parts = [] : map aux [1 ..]
  where
    aux n = [n] : [x : p | x <- [1 .. n], p <- parts !! (n - x), x <= head p]

-- λ> mapM_ print $ partitions 4
-- [4]
-- [1,3]
-- [1,1,2]
-- [1,1,1,1]
-- [2,2]
-----------------------------------------------------------------------------------
-- division by subtraction
--
dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy num den = loop num den 0
  where
    loop x y acc
      | x < y = (acc, x)
      | otherwise = loop (x - y) y (acc + 1)

-- λ> dividedBy 1024 41
-- (24,40)
-- concat the numbers of a list into individual numbers
concatNums :: [Int] -> [[Int]]
concatNums [] = [[]]
concatNums [x] = [[x]]
concatNums (x:y:ys) =
  map (x :) (concatNums (y : ys)) ++ concatNums ((10 * x + y) : ys)

-- λ> mapM_ print $ concatNums [1,2,3]
-- [1,2,3]
-- [1,23]
-- [12,3]
-- [123]
-----------------------------------------------------------------------------------
--
-- an implementation of the foldr function
-- This is right associative. In the  case of lists when foldr is applied
-- to a binary  operator, a starting value  (typically the right-identity
-- of  the  operator) and  a  list  will  reduce  the list  using  binary
-- operator, from right to left:
--
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
--
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc xs =
  case xs of
    []     -> acc
    (y:ys) -> f y (foldr f acc ys)

-- visualizing the foldr function
showFoldr :: Int -> String
showFoldr n =
  foldr (\x y -> concat ["( ", x, " + ", y, " )"]) "0" $ map show [1 .. n]

-- λ> showFoldr 5
-- "( 1 + ( 2 + ( 3 + ( 4 + ( 5 + 0 ) ) ) ) )"
-- λ> mapM_ print $ scanr (\x y -> concat ["( ", x, " + ", y, " )"]) "0" $ map show [1 .. 5]
-- "( 1 + ( 2 + ( 3 + ( 4 + ( 5 + 0 ) ) ) ) )"
-- "( 2 + ( 3 + ( 4 + ( 5 + 0 ) ) ) )"
-- "( 3 + ( 4 + ( 5 + 0 ) ) )"
-- "( 4 + ( 5 + 0 ) )"
-- "( 5 + 0 )"
-- "0"
-- An implementation of foldl
-- This is left  associative. In the case of lists  when foldl is applied
-- to a binary operator, a starting value (typically the left-identity of
-- the  operator), and  a  list will  reduce the  list  using the  binary
-- operator from left to right:
--
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
--
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc xs =
  case xs of
    []     -> acc
    (y:ys) -> foldl f (f acc y) ys

-- visualizing the left fold
showFoldl :: Int -> String
showFoldl n =
  foldl (\x y -> concat ["( ", x, " + ", y, " )"]) "0" $ map show [1 .. n]

-- λ> showFoldl 5
-- "( ( ( ( ( 0 + 1 ) + 2 ) + 3 ) + 4 ) + 5 )"
-- λ> mapM_ print $ scanl (\x y -> concat ["( ", x, " + ", y, " )"]) "0" $ map show [1 .. 5]
-- "0"
-- "( 0 + 1 )"
-- "( ( 0 + 1 ) + 2 )"
-- "( ( ( 0 + 1 ) + 2 ) + 3 )"
-- "( ( ( ( 0 + 1 ) + 2 ) + 3 ) + 4 )"
-- "( ( ( ( ( 0 + 1 ) + 2 ) + 3 ) + 4 ) + 5 )"
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr step []
  where
    step y ys
      | p y = y : ys
      | otherwise = ys

identity :: [a] -> [a]
identity = foldr (:) []

suffixes :: [a] -> [[a]]
suffixes xs@(y:ys) = xs : suffixes ys
suffixes _         = []

-----------------------------------------------------------------------------------
-- a simple Towers of Hanoi
data Tower
  = A
  | B
  | C
  deriving (Show)

move :: Int -> Tower -> Tower -> Tower -> [(Tower, Tower)]
move 0 _ _ _ = []
move n src dst aux =
  move (n - 1) src aux dst ++ [(src, dst)] ++ move (n - 1) aux dst src

showMoves :: Int -> IO ()
showMoves n = print $ move n A B C

-- λ> showMoves 3
-- [(A,B),(A,C),(B,C),(A,B),(C,A),(C,B),(A,B)]
-----------------------------------------------------------------------------------
-- prime numbers using Sieve of Eratosthenes
primesList1 :: [Integer]
primesList1 = sieve [2 ..]
  where
    sieve :: (Integral a) => [a] -> [a]
    sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- an efficient approach for generating the primes using sieve
primesList2 :: [Integer]
primesList2 = sieve [2 ..]
  where
    sieve (x:xs) = x : sieve (remove x xs)
      where
        remove z (y:ys)
          | y < z = y : remove z ys
          | y == z = remove (z + x) ys
          | y > z = y : remove (z + x) ys

getPrimes :: Int -> IO ()
getPrimes n = print $ take n primesList1

-----------------------------------------------------------------------------------
--
-- memoized version of Fibonacci numbers
fib :: Int -> Int
fib i = fiblist !! i

fiblist :: [Int]
fiblist = map fun [0 ..]
  where
    fun 0 = 0
    fun 1 = 1
    fun j = fiblist !! (j - 1) + fiblist !! (j - 2)

-- factors of a number
factors :: (Integral a) => a -> [a]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- λ> factors 18
-- [1,2,3,6,9,18]
-- primality check using the above function
isprime :: (Integral a) => a -> Bool
isprime x = factors x == [1, x]

-- get prime numbers till an upper limit
primes :: (Integral a) => a -> [a]
primes i = [x | x <- [2 .. i], isprime x]

-- λ> primes 21
-- [2,3,5,7,11,13,17,19]
-- get all the positions of a value in a list
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [p | (i, p) <- zip xs [0 .. j], x == i]
  where
    j = length xs - 1

-- λ> positions 1 [0,1,1,0,1,0,0,1]
-- [1,2,4,7]
-- list of all perfect numbers upto a limit
-- A positive number is a number for which the sum of  all its factors
-- excluding the number itself is equal to the number.
-- predicate to check if a number is perfect
isperfect :: (Integral a) => a -> Bool
isperfect n = sum (factors n) == 2 * n

-- get perfect numbers upto the limit
perfects :: (Integral a) => a -> [a]
perfects x = filter isperfect [1 .. x]

-----------------------------------------------------------------------------------
-- pairs of parentheses
parens :: Int -> [String]
parens n = func 0 0 [] []
  where
    func a b x y
      | a == n && b == n = reverse x : y
      | otherwise =
        if b < a
          then func a (b + 1) ('}' : x) z
          else z
      where
        z =
          if a < n
            then func (a + 1) b ('{' : x) y
            else y

-- λ> mapM_ print $ parens 3
-- "{}{}{}"
-- "{}{{}}"
-- "{{}}{}"
-- "{{}{}}"
-- "{{{}}}"
-----------------------------------------------------------------------------------
-- translate char in set1 to corresponding char in set2
translate :: String -> String -> Char -> Char
translate [] _ c = c
translate (x:xs) [] c =
  if x == c
    then ' '
    else translate xs [] c
translate (x:xs) [y] c =
  if x == c
    then y
    else translate xs [y] c
translate (x:xs) (y:ys) c =
  if x == c
    then y
    else translate xs ys c

-- translate an entire string
translateStr :: String -> String -> String -> String
translateStr str1 str2 = map (translate str1 str2)

-- pairs
newtype Pair =
  Pair (Int, Int)
  deriving (Eq)

instance Num Pair where
  Pair (a, b) + Pair (x, y) = Pair (a + x, b + y)
  Pair (a, b) - Pair (x, y) = Pair (a - x, b - y)
  Pair (a, b) * Pair (x, y) = Pair (a * b, x * y)
  negate (Pair (a, b)) = Pair (negate a, negate b)
  abs (Pair (a, b)) = Pair (abs a, abs b)
  signum (Pair (a, b)) = Pair (signum a, signum b)
  fromInteger 0 = Pair (0, 0)

-- all positive pairs
instance Ord Pair where
  Pair (a, b) > 0 = min a b >= 0 && max a b > 0

instance Show Pair where
  show (Pair (a, b)) = "(" ++ show a ++ ", " ++ show b ++ ")"

posRange :: Int -> Int -> [Pair]
posRange a b
  | a < 0 || b < 0 = []
  | otherwise = tail [Pair (x, y) | x <- [0 .. a], y <- [0 .. b]]

-- λ> mapM_ print $ posRange 2 3
-- (0, 1)
-- (0, 2)
-- (0, 3)
-- (1, 0)
-- (1, 1)
-- (1, 2)
-- (1, 3)
-- (2, 0)
-- (2, 1)
-- (2, 2)
-- (2, 3)
-----------------------------------------------------------------------------------
-- | playing with matrices
--
type Vector = [Int]

type Matrix = [Vector]

-- | number of rows of a matrix
numRows :: Matrix -> Int
numRows = length

-- | number of columns of a matrix
numCols :: Matrix -> Int
numCols = length . head

-- | define a zero vector
zeroVector :: Int -> Vector
zeroVector n = replicate n 0

-- | vector scalar product
vectorScalarProduct :: Int -> Vector -> Vector
vectorScalarProduct n vec = [n * x | x <- vec]

-- | matrix scala product
matrixScalarProduct :: Int -> Matrix -> Matrix
matrixScalarProduct n mat = [vectorScalarProduct n vec | vec <- mat]

-- | adding two vectors
vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith (+)

-- | matrix addition
matrixSum :: Matrix -> Matrix -> Matrix
matrixSum = zipWith vectorSum

-- | vector dot product
dotProduct :: Vector -> Vector -> Int
dotProduct v1 v2 = sum $ zipWith (*) v1 v2

-- | transpose of a matrix
transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose x      = map head x : transpose (map tail x)

-- | matrix product
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct a b = [map (dotProduct row) (transpose b) | row <- a]

-- | deteerminant of a matrix
--   create an adjoint matrix first; for ith row and jth column
--   remove the corresponding row, column from a matrix
--
--   define a helper function
cut :: [a] -> Int -> [a]
cut [] _ = []
cut xs n
  | n < 1 || n > length xs = xs
  | otherwise = take (n - 1) xs ++ drop n xs

mremove :: Matrix -> Int -> Int -> Matrix
mremove mat i j
  | null mat || i < 1 || i > numRows mat || j < 1 || j > numCols mat =
    error "OutOfIndex"
  | otherwise = transpose (cut (transpose (cut mat i)) j)

deteerminant :: Matrix -> Int
deteerminant [] = error "Determinant of a Zero Matrix"

determinant [[x]] = x
determinant m =
  sum
    [ (-1) ^ (j + 1) * head m !! (j - 1) * determinant (mremove m 1 j)
    | j <- [1 .. (numCols m)]
    ]

-- | matrix of co-factors
cofactor :: Matrix -> Int -> Int -> Int
cofactor mat i j = (-1) ^ (i + j) * determinant (mremove mat i j)

cofactorMatrix :: Matrix -> Matrix
cofactorMatrix mat = [[cofactor mat i j | j <- [1 .. n]] | i <- [1 .. n]]
  where
    n = length mat

-- | inverse of a matrix
-----------------------------------------------------------------------------------
--
-- Countdown problem
-- create a number using other numbers and mathematical expressions
-- similar to the problem described at the below link but with more
-- number of expressions that + and -
-- http://www.cut-the-knot.org/do_you_know/digits.shtml
--
data Exp
  = Num Int
  | Add Exp
        Exp
  | Mul Exp
        Exp
  | Sub Exp
        Exp
  | Div Exp
        Exp
  deriving (Eq)

instance Show Exp where
  show (Num n)   = show n
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"

-- evaluate the abstract expression
eval :: Exp -> Int
eval (Num n)   = n
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

-- all possible ways of selecting a number from a list
--
pick :: [a] -> [(a, [a])]
pick [] = []
pick z  = split z []

split :: [a] -> [a] -> [(a, [a])]
split [] _       = []
split (x:xs) acc = (x, acc ++ xs) : split xs (acc ++ [x])

-- list of all possible binary operations between two numbers
-- x and y considering x, y as tree branches
listOfBins :: Exp -> Exp -> [Exp]
listOfBins x y = [Add x y, Sub x y, Sub y x, Mul x y, Div x y, Div y x]

-- all valid combinations of number operations
-- this eliminates any duplicate expressions
check :: Exp -> Bool
check (Num x) = True
check (Add x y) = check x && check y && eval x > eval y
check (Sub x y) = check x && check y && (eval x - eval y) >= 0
check (Mul x y) = check x && check y && eval x > eval y
check (Div x y) = check x && check y && eval y > 0 && (eval x `mod` eval y) == 0

-- once the list of numbers are split into pairs with left and right branches
-- we can consider the left branch as given number and right branch can be
-- used to generate subree.
expressions :: [Int] -> [Exp]
expressions [x] = [Num x]
expressions x = concatMap allExpressions subtree
  where
    subtree = map (\(y, rest) -> (Num y, expressions rest)) (pick x)
    allExpressions (p, q) = concatMap (listOfBins p) q

-- λ> filter (\x -> check x && eval x == 3) (expressions [1,2,3])
-- [(3 * (2 - 1)),(3 / (2 - 1)),(3 * (2 - 1)),(3 / (2 - 1))]
getVal :: Int -> [Int] -> [Exp]
getVal x y = filter (\expr -> check expr && eval expr == x) (expressions y)

-- λ> mapM_ print $ getVal 14 [2,5,1,9]
-- (9 + (5 * (2 - 1)))
-- (9 + (5 / (2 - 1)))
-- (9 + (5 * (2 - 1)))
-- (9 + (5 / (2 - 1)))
-----------------------------------------------------------------------------------
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations _ []     = []
allCombinations f (x:xs) = foldl (liftA2 f) x xs

-- λ> allCombinations (+) [[0,1],[2,3,4]]
-- [2,3,4,3,4,5]
-----------------------------------------------------------------------------------
-- lifting functions
lift :: (Applicative f) => (a -> b) -> f a -> f b
lift f x = pure f <*> x

-- lifting a binary function
lift2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
lift2 f x y = fmap f x <*> y

-- lifting a ternary function
lift3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 f x y z = fmap f x <*> y <*> z

-- monadic version of map
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence (fmap f xs)

-- Ackerman function
ackerman :: (Integral a) => a -> a -> a
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

-- λ> ackerman 3 3
-- 61
-----------------------------------------------------------------------------------
{-
Adler 32 algorithm

An Adler-32 checksum is obtained by calculating two 16-bit checksums A
and B and concatenating their bits into a 32-bit integer. A is the sum
of  all bytes  in  the  stream plus  one,  and B  is  the  sum of  the
individual values of A from each step.

At the  beginning of  an Adler-32  run, A  is initialized  to 1,  B to
0. The  sums are done modulo  65521 (the largest prime  number smaller
than 2¹⁶). The  bytes are  stored in  network order  (big endian),  B
occupying the two most significant bytes.

The function can be expressed as below
A = 1 + D₁ + D₂ + D₃ + ... + Dₙ (moδ 65521)
  B = (1 + D₁) + (1 + D₁ + D₂) + ... + (1 + D₁ + D₂ + ... + Dₙ) (moδ 65521)
    = n•D₁ + (n-1)•D₂ + (n-2)•D₃ + ... + Dₙ + n (moδ 65521)

Adler-32(D) = B • 65521 + A
-}
-- Calculates a two's complement integer from the given input value's bits
twosComplement :: Int -> Int -> Int
twosComplement inputVal numBits =
  let mask = 2 ^ (numBits - 1)
  in complement (inputVal .&. mask) + inputVal .&. complement mask

adler32 :: String -> Int
adler32 = loop 1 0
  where
    loop a b (x:xs) =
      let l = (a + (C.ord x .&. 0xff)) `mod` 65521
          m = (l + b) `mod` 65521
      in loop l m xs
    loop a b _ = (b `shiftL` 16) .|. a

-----------------------------------------------------------------------------------
data List a
  = Cons a
         (List a)
  | Nil
  deriving (Show, Eq)

fromList :: [a] -> List a
fromList = foldr Cons Nil

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse x (y:ys) = y : prepend x ys
  where
    prepend _ []     = []
    prepend s (t:ts) = s : t : prepend s ts

-- read contents from a file, apply a function to it and write
-- contents to antoher file
interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith fun inFile outFile = do
  input <- readFile inFile
  writeFile outFile (fun input)

out :: IO ()
out = mainWith myFun
  where
    mainWith fun = do
      args <- getArgs
      case args of
        [input, output] -> interactWith fun input output
        _               -> putStrLn "error: 2 args are required"
    myFun = id

-----------------------------------------------------------------------------------
-- a simple 6 sided dice example
data Die
  = S1
  | S2
  | S3
  | S4
  | S5
  | S6

instance Show Die where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

instance Eq Die where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _   = False

instance Ord Die where
  S6 `compare` S6 = EQ
  S6 `compare` _ = GT
  _ `compare` S6 = LT
  S5 `compare` S5 = EQ
  S5 `compare` _ = GT
  S4 `compare` S4 = EQ
  S4 `compare` S5 = LT
  S4 `compare` _ = GT
  S3 `compare` S3 = EQ
  S3 `compare` S2 = GT
  S3 `compare` S1 = GT
  S3 `compare` _ = LT
  S2 `compare` S2 = EQ
  S2 `compare` S1 = GT
  S2 `compare` _ = LT
  S1 `compare` S1 = EQ
  S1 `compare` _ = LT

instance Enum Die where
  toEnum 0 = S1
  toEnum 1 = S2
  toEnum 2 = S3
  toEnum 3 = S4
  toEnum 4 = S5
  toEnum 5 = S6
  toEnum _ = error "No such dice side"
  fromEnum S1 = 0
  fromEnum S2 = 1
  fromEnum S3 = 2
  fromEnum S4 = 4
  fromEnum S5 = 4
  fromEnum S6 = 5

-- rot 13 algorithm generalized fpr any N
-- define a 4 letter alphabet first
data FourLetterAlpha
  = L1
  | L2
  | L3
  | L4
  deriving (Show, Enum, Bounded)

-- generic rotN function
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN num ch = toEnum rotation
  where
    half = num `div` 2
    offset = fromEnum ch + half
    rotation = offset `mod` num

-- λ> fmap (rotN 4) [L1 ..]
-- [L3,L4,L1,L2]
-- get the number representing the largest character
getLargestCharNum :: Int
getLargestCharNum = fromEnum (maxBound :: Char)

-- rotating a single character
rotChar :: Char -> Char
rotChar = rotN nChar
  where
    nChar = 1 + fromEnum (maxBound :: Char)

-- encoding the 4 letter alphabetic message
fourLetterEnc :: [FourLetterAlpha] -> [FourLetterAlpha]
fourLetterEnc = fmap rot4l
  where
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlpha)
    rot4l = rotN alphaSize

-- λ> fourLetterEnc [L2,L4,L1,L1,L3,L2]
-- [L4,L2,L3,L3,L1,L4]
-- ideally the rotN algorithm should be  symmetrical if we apply the same
-- rotN twice,  it should return the  original but in   our  case it only
-- works  for even  number of  letters due  to the  integer division  and
-- rounding applied in the function. To  fix this we can create a similar
-- function to original rotN  which adds a one offset if  there is an odd
-- number of letters in the alphabet.
rotNdecode :: (Bounded a, Enum a) => Int -> a -> a
rotNdecode n c = toEnum rotation
  where
    half = n `div` 2
    offset =
      if even n
        then fromEnum c + half
        else 1 + fromEnum c + half
    rotation = offset `mod` n

data ThreeLetterAlpha
  = Alpha
  | Beta
  | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMsg :: [ThreeLetterAlpha]
threeLetterMsg = [Alpha, Alpha, Kappa, Beta, Kappa, Alpha]

threeLetterEnc :: [ThreeLetterAlpha] -> [ThreeLetterAlpha]
threeLetterEnc = map rot3l
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlpha)
    rot3l = rotN alphaSize

threeLetterDec :: [ThreeLetterAlpha] -> [ThreeLetterAlpha]
threeLetterDec = map rot3ldecode
  where
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlpha)
    rot3ldecode = rotNdecode alphaSize

-- λ> threeLetterDec $ threeLetterEnc threeLetterMsg
-- [Alpha,Alpha,Kappa,Beta,Kappa,Alpha]
-- λ> threeLetterMsg
-- [Alpha,Alpha,Kappa,Beta,Kappa,Alpha]
-- XOR boolean values
xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && not (v1 && v2)

-- operate over a pair of booleans
xorPair :: (Bool, Bool) -> Bool
xorPair (b1, b2) = b1 `xorBool` b2

-- actual xor function
xor :: [Bool] -> [Bool] -> [Bool]
xor = zipWith (curry xorPair)

type BitList = [Bool]

maxBits :: Int
maxBits = length $ int2bit maxBound

int2bit :: Int -> [Int]
int2bit 0 = []
int2bit i = b : int2bit a
  where
    (a, b) = i `divMod` 2

-- convert an integer into bits with the following mapping
-- True = 1
-- False = 0
-- also the result is expanded with leading 0's or falses upto maxBound
intToBits :: Int -> BitList
intToBits i = map (/= 0) bits
  where
    ibits = int2bit i
    missingBits = maxBits - length ibits
    leadingBits = replicate missingBits 0
    bits = reverse (ibits ++ leadingBits)

-- convert a single character to bits
charToBits :: Char -> BitList
charToBits c = intToBits (fromEnum c)

-- conversion from bit list back to int
bitsToInt :: BitList -> Int
bitsToInt bits = sum $ map (\x -> 2 ^ fst x) list
  where
    size = length bits
    list = filter snd $ zip [size - 1,size - 2 .. 0] bits

-- λ> bitsToInt $ intToBits (maxBound :: Int)
-- 9223372036854775807
-- converting from bits to character
bitsToChar :: BitList -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- One Time Pad (otp)
-- this involves XOR of a plain text with a secret text called pad
-- the resulting text will be secretly encoded and can be craked by
-- the original pad with XOR
-- apply the XOR to plain text and pad
apply :: String -> String -> [BitList]
apply plain pad = map (uncurry xor) $ zip plainBits paddedBits
  where
    plainBits = map charToBits plain
    paddedBits = map charToBits pad

-- finally convert the bits list to String
applyOTP :: String -> String -> String
applyOTP plain pad = map bitsToChar (apply plain pad)
-----------------------------------------------------------------------------------

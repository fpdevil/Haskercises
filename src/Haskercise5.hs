{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise5.hs
Description: General Monad examples and exercises in Haskell
             covering the three standard monads as follows
             Writer Monad
             Reader Monad
             State Monad
-}

module Haskercise5 where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Monoid          ()
import           System.Random
-----------------------------------------------------------------------------------
-- | simulating a Writer Monad
--   learning from LYAH
-----------------------------------------------------------------------------------
isBig :: Int -> (Bool, String)
isBig x = if x > 9 then (True, show x ++ " is greater than 9") else (False, " less than 9")

-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog (x, xlog) f = (y, xlog ++ ylog)
--                        where
--                        (y, ylog) = f x
-- λ> ("Mickey", "Heading for the Cartoon.") `applyLog` (\x -> (length x, "got the length."))
-- (6,"Heading for the Cartoon.got the length.")

-- rewriting using monoids to handle wide data formats
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, xlog) f = (y, ylog `mappend` xlog)
                       where
                       (y, ylog) = f x

-- example
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "brownie" = ("Coffee", Sum 9)
addDrink "cereals" = ("Milk", Sum 12)
addDrink "cheese"  = ("Wine", Sum 21)
addDrink _         = ("Hot Water", Sum 3)

-- λ> ("cereals", 10) `applyLog` addDrink
-- ("Milk",Sum {getSum = 22})
-- λ> ("cereals", 10) `applyLog` addDrink `applyLog` addDrink
-- ("Hot Water",Sum {getSum = 25})

powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset (x : xs) = powerset xs >>= \set -> [set, x : set]

-----------------------------------------------------------------------------------
-- | WRITER MONAD
-----------------------------------------------------------------------------------
{-
definition of the Writer Monad in standard libraries
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
   return x = Writer (x, mempty)
   (Writer (x, xval)) >>= f = Writer (y, xval `mappend` yval)
                              where
                                Writer (y, yval) = f x


some one liners for testing
λ> (return ['a' .. 'j']) :: Writer [Int] String
    WriterT (Identity ("abcdefghij", []))
λ> tell [1 .. 5] >> tell [6 .. 10] >> (return ['a' .. 'j']) :: Writer [Int] String
    WriterT (Identity ("abcdefghij", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
λ> runWriter (tell [1 .. 5] >> tell [6 .. 10] >> (return ['a' .. 'j']) :: Writer [Int] String)
    ("abcdefghij", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
-}

-----------------------------------------------------------------------------------
-- | logging multiplication of numbers
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Received number: " ++ show x])

multiplyWithLog :: Int -> Int -> Writer [String] Int
multiplyWithLog x y = do
   a <- logNumber x
   b <- logNumber y
   tell ["Multiplying the nums " ++ show x ++ " and " ++ show y]
   return (a * b)

-- | gcd of numbers with logging
gcdW :: Int -> Int -> Writer [String] Int
gcdW a b
   | b == 0 = do
      tell ["completed with " ++ show a]
      return a
   | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcdW b (a `mod` b)

-- λ> mapM_ print $ snd $ runWriter (gcdW 1221 2044)
-- "1221 mod 2044 = 1221"
-- "2044 mod 1221 = 823"
-- "1221 mod 823 = 398"
-- "823 mod 398 = 27"
-- "398 mod 27 = 20"
-- "27 mod 20 = 7"
-- "20 mod 7 = 6"
-- "7 mod 6 = 1"
-- "6 mod 1 = 0"
-- "completed with 1"


-- | filtering small elements
keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 5 = do
      tell ["Keeping " ++ show x]
      return True
  | otherwise = do
      tell [show x ++ " is too large, throwing away..."]
      return False

--λ> mapM_ print $ snd $ runWriter $ filterM keepSmall [7,6,2,14,9,4,18,21,0]
--"7 is too large, throwing away..."
--"6 is too large, throwing away..."
--"Keeping 2"
--"14 is too large, throwing away..."
--"9 is too large, throwing away..."
--"Keeping 4"
--"18 is too large, throwing away..."
--"21 is too large, throwing away..."
--"Keeping 0"

-- | binary power
binpow :: Int -> Int -> Writer [String] Int
binpow 0 _ = return 1
binpow n x
   | even n    = binpow (n `div` 2) x >>= \y ->
                 writer (y * y, ["Square " ++ show y])
   | otherwise = binpow (n - 1) x >>= \y ->
                 writer (x * y, ["Multiplying " ++ show x ++ " and " ++ show y])


-- λ> mapM_ print $ execWriter $ binpow 5 6 >> binpow 4 2
-- "Multiplying 6 and 1"
-- "Square 6"
-- "Square 36"
-- "Multiplying 6 and 1296"
-- "Multiplying 2 and 1"
-- "Square 2"
-- "Square 4"


-- | simple fibonacci with logger
fibw :: Int -> Writer [String] Int
fibw n
   | n < 2 = do
      tell ["fibw " ++ show n ++ " = 1"]
      return 1
   | otherwise = do
      a <- fibw (n - 1)
      b <- fibw (n - 2)
      tell ["fibw " ++ show n ++ " = " ++ show (a + b)]
      return (a + b)

-- λ> mapM_ print $ snd $ runWriter (fibw 5)
-- "fibw 1 = 1"
-- "fibw 0 = 1"
-- "fibw 2 = 2"
-- "fibw 1 = 1"
-- "fibw 3 = 3"
-- "fibw 1 = 1"
-- "fibw 0 = 1"
-- "fibw 2 = 2"
-- "fibw 4 = 5"
-- "fibw 1 = 1"
-- "fibw 0 = 1"
-- "fibw 2 = 2"
-- "fibw 1 = 1"
-- "fibw 3 = 3"
-- "fibw 5 = 8"

-----------------------------------------------------------------------------------
-- | READER MONAD
-----------------------------------------------------------------------------------
{-
Reader monad is for computations which read values from a shared environment.
If a configuration needs to be passed around to a lot of functions, then we
can use a Reader Monad.

Here is how its defined in the standard haskell library

newtype Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
   return = Reader . const
   (Reader x) >>= f = Reader $ \r -> runReader (f (x r)) r
-}

mickey :: Reader String String
mickey = do
   env <- ask
   return (env ++ " This is Mickey.")

donald :: Reader String String
donald = do
   env <- ask
   return (env ++ " This is Donald.")

md :: Reader String String
md = do
   m <- mickey
   d <- donald
   return (m ++ "\n" ++ d)

runDisney :: String
runDisney = runReader md "Who is this?"

-- > putStr $ runDisney
-- Who is this? This is Mickey.
-- Who is this? This is Donald.
-----------------------------------------------------------------------------------
-- | STATE MONAD
-----------------------------------------------------------------------------------
{-
State Monad  is similar  to the  above, but  it can  Write as  well as
Read.  The state  monad  allows functions  within  a stateful  monadic
context to access and modify shared state.

A stateful computation can be represented in a purely-functional way as
a function from states to pairs of result and new state:

           f : state -> {result, state}

Here is how it's defined in standard haskell

newtype State s a = State { runState :: s -> (a, s) }
or more specifically as
newtype State state result = State { runState :: state -> (result, state) }

runState :: (State state result) -> (state -> (result,state))
runState (State f) = f

instance Monad (State s) where
   return x = State $ \s -> (x, s)
   (State h) >>= f = State $ \s -> let (a, newState) = h s
                                       (State g) = f a
                                   in g newState

ghci examples
λ> runState (return "abcdef") 101
    ("abcdef", 101)
λ> runState (get) 101
    (101, 101)
λ> runState (put 100) 101
    ((), 100)
-}

testState :: (String, Int)
testState = runState f 101 where
   f = do
      s <- get
      put (s + 1)
      return "abcdef"

-- λ> testState
-- ("abcdef",102)

-- stack using a State
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push i = state $ \xs -> ((), i : xs)

stackOp :: State Stack ()
stackOp = do
   x <- pop
   if x == 7
      then push 7
      else do
           push 3
           push 6

-- λ> runState stackOp [7,9,0,2,1,0]
-- ((),[7,9,0,2,1,0])
-- λ> runState stackOp [9,0,2,1,0]
-- ((),[6,3,0,2,1,0])

-- randomness in terms of State
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

-- | coin throw using the stateful randomness
coins :: State StdGen (Bool, Bool, Bool)
coins = do
   x <- randomSt
   y <- randomSt
   z <- randomSt
   return (x, y, z)

-- λ> runState coins (mkStdGen 1001)
-- ((True,False,False),460862966 2103410263)

-- | thread random generator state through multiple calls
--   to the generation function
data MT = MT Int Bool Char Int deriving Show

getRand :: (Random a) => State StdGen a
getRand = do
   gen <- get
   (x, nexgen) <- return (random gen)
   put nexgen
   return x

-- λ> runState getRand (mkStdGen 99)
-- (6777721729736976246,1494795594 2103410263)

-- | get random with bounds
getR :: (Random a) => (a, a) -> State StdGen a
getR bounds = do
   gen <- get
   (x, nexgen) <- return (randomR bounds gen)
   put nexgen
   return x

mkRandValue :: StdGen -> (MT, StdGen)
mkRandValue = runState $ do
   a <- getR (1, 100)
   b <- getRand
   c <- getR ('a', 'z')
   d <- getR (-a, a)
   return (MT a b c d)

-- λ> mkRandValue  (mkStdGen 1001)
-- (MT 36 False 'o' (-18),529366043 1872071452)
-----------------------------------------------------------------------------------
-- | generating a stream of unique labels
--   Suppose  we  need to  generate  labels  in  code, for  instance  while
--   performing operations on an abstract  syntax tree. Each label needs to
--   be unique,  and we need  labels in  various functions. In  haskell, we
--   cannot just mutate some counter-variable.

-- | state is managed here, which indicates that it contains an integer
type Label = State Int

-- | increment the internal state and return a label
inc :: Label String
inc = state $ \x -> let y = x + 1
                   in ("$" ++ show y, y)


-- | generate a pair of lables by lifting the inc
pairs :: Label (String, String)
pairs = (,) <$> inc <*> inc

-- | function for generating the labels
labels :: Bool -> Label [(String, String)]
labels predicate = func <$> pairs
                        <*> pairs
                        <*> pairs
                   where
                      func x y z = if predicate
                                      then [x, z]
                                      else [x, y, z]

-- | run the labels
runLabels :: IO ()
runLabels = do
   putStrLn "Enter `True` or `False`..."
   predicate <- getLine
   print $ evalState (labels . read $ predicate) 0

-- λ> runLabels
-- Enter `True` or `False`...
-- True
-- [("$1","$2"),("$5","$6")]
-- λ> runLabels
-- Enter `True` or `False`...
-- False
-- [("$1","$2"),("$3","$4"),("$5","$6")]

-- | Pairs of even integers
evenPair :: (Alternative m, Monad m, Integral a, Integral b) => m a -> m b -> m (a, b)
evenPair a b = a >>= \x ->
                   b >>= \y ->
                       guard (even x && even y) >>
                           return (x, y)

{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise2.hs
Description: General examples and exercises in Haskell

-}

module Haskercise2 where

import           Control.Applicative
import           Control.Monad       hiding (filterM, foldM, foldM_, mapM,
                                      mapM_, sequence, sequence_, unless, when,
                                      zipWithM)
import qualified Data.List           as L
import           Data.Semigroup
import           Prelude             hiding (sequence, sequence_)
import           System.Environment
import           System.Random       hiding (randoms)
-----------------------------------------------------------------------------------

-- sequence function re-implemented.
-- the function takes a list of monadic computations, executes one by one and returns
-- a list of the results; if any computation fails, the whole function fails
sequence :: (Monad m) => [m a] -> m [a]
sequence = foldr mcons (return [])
         where
         mcons x y = x >>= \w -> y >>= \z -> return (w : z)

-- sequence_ implementation
sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = foldr (>>) (return ())

-- λ> sequence [Just 1, Just 2, Just 3]
-- Just [1,2,3]
-- λ> sequence [Just 1, Nothing, Just 3]
-- Nothing

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence (map f xs)

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f xs = sequence_ (map f xs)

-- λ> mapM_ (\x -> when (not (null x)) (putStrLn x)) ["abc","","","def","","ghi"]
-- abc
-- def
-- ghi

-- a version of replicateM and replicateM_
repM :: (Monad m) => Int -> m a -> m [a]
repM n x = sequence (replicate n x)

repM_ :: (Monad m) => Int -> m a -> m ()
repM_ n x = sequence_ (replicate n x)

-- a version of foldM implented
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc []       = return acc
foldM f acc (x : xs) = f acc x >>= \y -> foldM f y xs

foldM_ :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ f acc xs = foldM f acc xs >> return ()

-- testing foldM
testfold :: (Num a, Show a, Enum a) => a -> IO a
testfold n = foldM (\x y -> putStrLn (show x ++ " + " ++ show y ++ " = " ++ show (x + y)) >> return (x + y)) 0 [1 .. n]

-- λ> testfold 6
-- 0 + 1 = 1
-- 1 + 2 = 3
-- 3 + 3 = 6
-- 6 + 4 = 10
-- 10 + 5 = 15
-- 15 + 6 = 21
-- 21


-- filterM function re-implemented
-- The filterM function  works like the list filter function  inside of a
-- monad. It takes a predicate function  which returns a Boolean value in
-- the monad and a  list of values. It returns, inside  the monad, a list
-- of those values for which the predicate was True.
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x : xs) =
        do y <- p x
           ys <- filterM p xs
           return (if y
                   then x : ys
                   else ys)


-- powerset example
-- λ> filterM (\_ -> [True, False]) [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-- zipWithM, the monadic version of zipWith
zipWithM :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

-- conditional monadic execution
when :: (Monad m) => Bool -> m () -> m ()
when p x = if p then x else return ()

unless :: (Monad m) => Bool -> m () -> m ()
unless p = when (not p)

-----------------------------------------------------------------------------------
-- from Learn You A Haskell
-----------------------------------------------------------------------------------
-- Plural variant of random, producing an infinite list of random values instead
-- of returning a new generator.
randoms :: (RandomGen g, Random a) => g -> [a]
randoms gen = value:randoms newGen
        where
        (value, newGen) = random gen

-- function that generates a finite stream of numbers and a new generator
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen = (value : rest, finalGen)
              where
              (value, newGen) = random gen
              (rest, finalGen) = finiteRandoms (n - 1) newGen

-- max, min range elements from a list
maxmin :: (Ord a) => [a] -> (a, a)
maxmin [x]      = (x, x)
maxmin (x : xs) = (if x > max_xs then x else max_xs, if x < min_xs then x else min_xs)
    where
        (max_xs, min_xs) = maxmin xs

-- user guess example
guess :: IO ()
guess = do
      gen <- getStdGen
      promptForNumber gen

promptForNumber :: StdGen -> IO ()
promptForNumber gen = do
                let (randomNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)
                putStr "Guess a number between 1 to 10!"
                numStr <- getLine
                unless (null numStr) $ do
                           let number = read numStr
                           if randomNum == number
                              then putStrLn "Correct guess !"
                              else putStr $ "incorrect guess, number was " ++ show randomNum
                           promptForNumber newGen
-----------------------------------------------------------------------------------
-- RPN Calculator
-- Reverse Polish Notation
rpnSolver :: String -> Float
rpnSolver = head . foldl foldFunction [] . L.words
         where
         -- foldFunction :: [String] -> String -> [String]
         foldFunction (x : y : xs) "+" = (x + y) : xs
         foldFunction (x : y : xs) "-" = (x - y) : xs
         foldFunction (x : y : xs) "*" = (x * y) : xs
         foldFunction (x : y : xs) "/" = (y / x) : xs
         foldFunction (x : y: xs) "^"  = (y ** x) : xs
         foldFunction (x : xs) "ln"    = log x : xs
         foldFunction xs "sum"         = [sum xs]
         foldFunction xs mathstr       = read mathstr : xs

-- λ> rpnSolver "10 10 10 10 sum 4 /"
-- 10.0
-----------------------------------------------------------------------------------
-- distance from Heathrow to London

data RoadSection = RoadSection { getA :: Int
                               , getB :: Int
                               , getX :: Int
                               } deriving (Show)

type RoadSystem = [RoadSection]

-- Road system from Heathrow to London
heathrowToLondon :: RoadSystem
heathrowToLondon = [RoadSection 50 10 30, RoadSection 5 90 20, RoadSection 40 2 25, RoadSection 10 8 0]

data Road = A | B | X deriving (Show)
type Path = [(Road, Int)]

roadSelection :: (Path, Path) -> RoadSection -> (Path, Path)
roadSelection (pathA, pathB) (RoadSection a b x) =
              let costA = sum $ map snd pathA
                  costB = sum $ map snd pathB
                  costAlongA = costA + a
                  costAcrossA = costB + b + x
                  costAlongB = costB + b
                  costAcrossB = costA + a + x
                  nextPathToA = if costAlongA <= costAcrossA
                                   then (A, a) : pathA
                                   else (X, x) : (B, b) : pathB
                  nextPathToB = if costAlongB <= costAcrossB
                                   then (B, b) : pathB
                                   else (X, x) : (A, a) : pathA
              in (nextPathToA, nextPathToB)


optimumPath :: RoadSystem -> Path
optimumPath roadSystem =
            let (optimalAPath, optimalBPath) = foldl roadSelection ([], []) roadSystem
            in if sum (map snd optimalAPath) <= sum (map snd optimalBPath)
                  then reverse optimalAPath
                  else reverse optimalBPath

-- group n elements of a list
groupN :: Int -> [a] -> [[a]]
groupN 0 _  = []
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)

main :: IO ()
main = do
     args <- getArgs
     contents <- readFile (head args)
     let set = groupN 3 (map read $ L.lines contents)
         roadSystem = fmap (\[a, b, x] -> RoadSection a b x) set
         path = optimumPath roadSystem
         pathStr = concatMap (show . fst) path
         pathCost = sum $ map snd path
     putStrLn $ "Optimal path to follow is " ++ pathStr
     putStrLn $ "Distance is " ++ show pathCost

-- λ> :main "paths.txt"
-- Optimal path to follow is BXAXBBX
-- Distance is 75
-----------------------------------------------------------------------------------
-- an efficient fibonacci number version using pairs
fibStep :: (Integral a) => (a, a) -> (a, a)
fibStep (x, y) = (y, x + y)

fibPair :: (Integral a) => a -> (a, a)
fibPair n
   | n == 0 = (0, 1)
   | otherwise = fibStep $ fibPair (n - 1)

fastFib :: (Integral a) => a -> a
fastFib = fst . fibPair
-----------------------------------------------------------------------------------
-- | a function that takes a list of applicatives and returns an
--   applicative that has a list as its result value
sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA []       = pure []
-- sequenceA (x : xs) = (:) <$> x <*> sequenceA xs
-- re-writing using foldr
sequenceA = foldr (\x -> (<*>) ((:) <$> x)) (pure [])

-- | using liftA2
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

-- λ> sequenceA [Just 3, Just 4, Just 5]
-- Just [3,4,5]

-- | another version of sequence using monad
mseq :: (Monad m) => [m a] -> m [a]
mseq [] = return []
mseq (x : xs) = do
     y <- x
     ys <- mseq xs
     return (y : ys)

-- | without the do notation
mseq' :: (Monad m) => [m a] -> m [a]
mseq' []       = return []
mseq' (x : xs) = x >>= (\y -> mseq' xs >>= \ys -> return (y : ys))
-----------------------------------------------------------------------------------
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _  = Nothing
applyMaybe (Just x) f = f x

-- | A knight's quest
--   depict knight's current position (col, row)
type Knight = (Int, Int)

-- | list of all positions to which a knight can move
knightMove :: Knight -> [Knight]
knightMove (c, r) = do
           (col, row) <- [(c + 2, r + 1), (c + 2, r - 1), (c - 2, r + 1), (c - 2, r - 1)
                        ,(c + 1, r + 2), (c - 1, r + 2), (c + 1, r - 2), (c - 1, r - 2)]
           guard (col `elem` [1 .. 8] && row `elem` [1 .. 8])
           return (col, row)

-- | all positions to which a knight can land in X moves
in3 :: Knight -> [Knight]
in3 start = do
    first <- knightMove start
    second <- knightMove first
    knightMove second

-- | same in monadic way
in3' :: Knight -> [Knight]
in3' start = knightMove start >>= knightMove >>= knightMove

-- | check that the knight can move from start to end positions in n moves
canReach3 :: Knight -> Knight -> Bool
canReach3 start end = end `elem` in3 start

-----------------------------------------------------------------------------------
-- | Tossing a Coin (H/T)
--   Probability Tables for Event and Probability
--   define type system for events and probabilities
type Events = [String]
type Probabilities = [Double]

data  ProbabilityTable = ProbabilityTable Events Probabilities

-- | create a probability table ensuring that sum of probabilities is 1
crtProTable :: Events -> Probabilities -> ProbabilityTable
crtProTable events probabilities = ProbabilityTable events normalizedP
   where
     totalP = sum probabilities
     normalizedP = map (/ totalP) probabilities

-- | create a string for showing single event and probability
showPair :: String -> Double -> String
showPair event probability = mconcat [event, " | ", show probability, "\n"]

-- | now make the ProbabilityTable an instance of Show
instance Show ProbabilityTable where
  show (ProbabilityTable events probabilities) = mconcat pairs
    where
      pairs = zipWith showPair events probabilities

-- λ> crtProTable ["Heads", "Tails"] [0.5, 0.5]
-- Heads|0.5
-- Tails|0.5

-- | cartesian product for clubbing probablities from multiple events
cartesianCombination :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianCombination f l1 l2 = zipWith f newL1 cycledL2
                               where
                                 -- repeat each elem in the first list for
                                 -- each elem in the second list
                                 nToAdd = length l2
                                 -- make nToAdd copies of element
                                 repeatedL1 = map (replicate nToAdd) l1
                                 newL1 = mconcat repeatedL1
                                 -- cycle second list so that it can be combined with first
                                 cycledL2 = cycle l2

-- | functions for combining events and combine probablitites
combineEvents :: Events -> Events -> Events
combineEvents = cartesianCombination func
                where
                  func x y = mconcat [x, "-", y]

combineProbs :: Probabilities -> Probabilities -> Probabilities
combineProbs = cartesianCombination (*)

-- | Make ProbabilityTable an instance of Semigroup
instance Semigroup ProbabilityTable where
  ptable1 <> (ProbabilityTable [] [])                  = ptable1
  (ProbabilityTable [] []) <> ptable2                  = ptable2
  (ProbabilityTable e1 p1) <> (ProbabilityTable e2 p2) = crtProTable newEvents newProbabilities
    where
      newEvents = combineEvents e1 e2
      newProbabilities = combineProbs p1 p2

-- | Make ProbabilityTable an instance of Monoid
instance Monoid ProbabilityTable where
  mempty = ProbabilityTable [] []
  mappend = (<>)
  -- not required to implement for mconcat

-- | testing with a fair coin
coin :: ProbabilityTable
coin = crtProTable ["Heads", "Tails"] [0.5, 0.5]

-- | a color spinner with various probabilities for each spinner
spinner :: ProbabilityTable
spinner = crtProTable ["Red", "Blue", "Green"] [0.3, 0.2, 0.5]

-- testing
-- λ> coin
-- Heads | 0.5
-- Tails | 0.5

-- λ> spinner
-- Red | 0.3
-- Blue | 0.2
-- Green | 0.5

-- below coming due to monoidal instance
-- λ> coin <> spinner
-- Heads-Red | 0.15
-- Heads-Blue | 0.1
-- Heads-Green | 0.25
-- Tails-Red | 0.15
-- Tails-Blue | 0.1
-- Tails-Green | 0.25

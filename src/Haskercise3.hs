{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise3.hs
Description: General Monad examples and exercises in Haskell

Sheep cloning experiment
Sheep have  parents; most  have two, but  cloned sheep  (e.g., Dolly)
have  only one,  and the  first  sheep (called  Adam and  Eve in  this
example) have no parents (or at  least their parents were not sheep!) So
the mother and father functions have to be of type Sheep -> Maybe Sheep

A Monad class definition in standard haskell mtl Contorl.Monad package

class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a

class (Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

MonadPlus laws:
          mzero >>= f         = mzero
          m >>= \x -> mzero   = mzero
          m `mplus` mzero     = m
          mzero `mplus` m     = m

List and Maybe can be an instance of both Monad and MonadPlus
some of the standard monadsad functions are re-implemented here
-}

module Haskercise3 where

import           Control.Applicative
import           Control.Monad       hiding (ap, filterM, foldM, foldM_, guard,
                                      join, mapM, mapM_, sequence, sequence_,
                                      unless, when, zipWithM, zipWithM_)
import           Data.Maybe
import           Prelude             hiding (mapM, mapM_, sequence, sequenceA,
                                      sequence_)

-----------------------------------------------------------------------------------
-- Sheep cloning experiment
-----------------------------------------------------------------------------------
-- definitions and type declarations
-- type Sheep = String

-- father :: Sheep -> Maybe Sheep
-- father = undefined

-- mother :: Sheep -> Maybe Sheep
-- mother = undefined

data Sheep = Sheep { name   :: String
                   , mother :: Maybe Sheep
                   , father :: Maybe Sheep
                   } deriving Show

-- parent sheep (father or mother) as a Maybe Monad
parentM :: Sheep -> Maybe Sheep
parentM sh = father sh `mplus` mother sh

-- parent sheep as a List Monad
parentL :: Sheep -> [Sheep]
parentL sh = maybeToList (mother sh) `mplus` maybeToList (father sh)

-- if the type definition of the Sheep is changed including MonadPlus
-- then the definition is applicable for both Maybe as well  as List
-- helper function for converting to Monad
toMonad :: (MonadPlus m) => Maybe a -> m a
toMonad Nothing  = mzero
toMonad (Just x) = return x

-- parent of the sheep using MonadPlus
parent :: (MonadPlus m) => Sheep -> m Sheep
parent sh = toMonad (father sh) `mplus` toMonad (mother sh)

-- build the sheep family tree for dolly
sheepFamily :: Sheep
sheepFamily = let adam   = Sheep "Adam" Nothing Nothing
                  eve    = Sheep "Eve" Nothing Nothing
                  uranus = Sheep "Uranus" Nothing Nothing
                  gaea   = Sheep "Gaea" Nothing Nothing
                  kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                  holly  = Sheep "Holly" (Just eve) (Just adam)
                  roger  = Sheep "Roger" (Just eve) (Just kronos)
                  molly  = Sheep "Molly" (Just holly) (Just roger)
               in
               Sheep "Dolly" (Just molly) Nothing


-- since the sheep is cloned, it may not always have mother and father
-- define a function which can combine operations to return a Maybe
apply :: Maybe a -> (a -> Maybe b) -> Maybe b
apply Nothing _  = Nothing
apply (Just x) f = f x


-- Naive implementations
-- now maternal grand father can be defined using above apply function
maternalGrandFatherNaive :: Sheep -> Maybe Sheep
maternalGrandFatherNaive sh = Just sh `apply` mother `apply` father

fathersMaternalGrandMotherNaive :: Sheep -> Maybe Sheep
fathersMaternalGrandMotherNaive sh = Just sh `apply` father `apply` mother `apply` mother

mothersPaternalGrandFatherNaive :: Sheep -> Maybe Sheep
mothersPaternalGrandFatherNaive sh = Just sh `apply` mother `apply` father `apply` father

-- print dolly's maternal grand father
showDollyMaternalGrandfather :: IO ()
showDollyMaternalGrandfather = let dolly = sheepFamily
                               in
                               print (maternalGrandFatherNaive dolly)


-- since Maybe is already an instance of Monad, we can use the standard haskell
-- monad operators to build complex operations
maternalGrandFather :: Sheep -> Maybe Sheep
maternalGrandFather sh = mother sh >>= father

-- simplified using the left identity law of Monads
-- maternalGrandFather sh = return sh >>= mother >>= father

paternalGrandFather :: Sheep -> Maybe Sheep
paternalGrandFather sh = father sh >>= father

maternalGrandMother :: Sheep -> Maybe Sheep
maternalGrandMother sh = mother sh >>= mother

paternalGrandMother :: Sheep -> Maybe Sheep
paternalGrandMother sh = father sh >>= mother

fathersMaternalGrandMother :: Sheep -> Maybe Sheep
fathersMaternalGrandMother sh = father sh >>= mother >>= mother

mothersPaternalGrandFather :: Sheep -> Maybe Sheep
mothersPaternalGrandFather sh = mother sh >>= father >>= father

-- grand parents of sheep
grandparent :: Sheep -> Maybe Sheep
grandparent sh = paternalGrandMother sh `mplus`
                 paternalGrandFather sh `mplus`
                 maternalGrandMother sh `mplus`
                 maternalGrandFather sh

-- grand parents interms of list
grandparentL :: Sheep -> [Sheep]
grandparentL sh = parentL sh >>= parentL

-- some standard haskell functions redefined
-- sequence
-- this function  takes a list of  wrapped computations, executes each  one in turn
-- and then returns a list of results
-- using do notation
sequenceDo :: (Monad m) => [m a] -> m [a]
sequenceDo [] = return []
sequenceDo (x : xs) = do
    y <- x
    ys <- sequenceDo xs
    return (y : ys)


--without the do notation in a monadic way
sequence :: (Monad m) => [m a] -> m [a]
sequence []       = return []
sequence (x : xs) = x >>= \y -> sequence xs >>= \ys -> return (y : ys)

-- λ> sequence [Just 1, Just 2, Just 3]
--    Just [1, 2, 3]

-- sequence function written using the Applicative functor
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA []       = pure []
-- sequenceA (x : xs) = pure (:) <*> x <*> sequenceA xs
sequenceA (x : xs) = foldr (\x -> (<*>) ((:) <$> x)) (pure []) xs

--
-- λ> sequenceA [print 1, print 2, print 3]
-- 1
-- 2
-- 3
-- [(), (), ()]

-- sequence_
-- this function has same behaviour as sequence, but it does not
-- return the list of results. useful only when side effects of
-- monadic computing are important
sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = foldr (>>) (return ())

-- λ> sequence [print 1, print 2, print 3]
-- 1
-- 2
-- 3
-- [(),(),()]
-- λ> sequence_ [print 1, print 2, print 3]
-- 1
-- 2
-- 3

-- mapping functions
-- mapM - this function maps a monadic computation over a list of
-- values and returns back list of results
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence (map f xs)

-- mapM_ - this function has same behaviour as mapM, but it does
-- not return the list of results.
mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f xs = sequence_ (map f xs)

-- λ> mapM Just [1,2,3]
-- Just [1,2,3]
-- λ> mapM_ Just [1,2,3]
-- Just ()
-- λ> mapM print [1,2,3]
-- 1
-- 2
-- 3
-- [(),(),()]
-- λ> mapM_ print [1,2,3]
-- 1
-- 2
-- 3

-- monadic version of fold function
-- foldM - this function is a monadic version of foldl in which
-- monadic computations built from a list are bound left-to-right
foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM _ acc []       = return acc
foldM f acc (x : xs) = f acc x >>= \y -> foldM f y xs

-- λ> foldM (\x y -> [y] : [x]) [] [1,2,3]
--     [[3], [2], [3], [1], [3], [2], [3], []]

-- foldM_ - it is same as foldM, but discards results
foldM_ :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
foldM_ f acc xs = void (foldM f acc xs)
-- foldM_ f acc xs = foldM f acc xs >> return ()


-- an example implementation using foldM
showFM :: (Integral a, Show a) => a -> IO a
showFM n = foldM (\x y ->
                    putStrLn (show x ++ " + " ++ show y ++ " = " ++ show (x + y))
                 >> return (x + y)) 0 [1 .. n]

-- λ> showFM 5
-- 0 + 1 = 1
-- 1 + 2 = 3
-- 3 + 3 = 6
-- 6 + 4 = 10
-- 10 + 5 = 15
-- 15

-- define a function to trace the ancestor(s) of a sheep
traceSheep :: Sheep -> [Sheep -> Maybe Sheep] -> Maybe Sheep
traceSheep = foldM getAncestor
             where
                getAncestor s f = f s

-- redefining the above using builtin flip and Monads
traceFamily :: (Monad m) => [Sheep -> m Sheep] -> Sheep -> m Sheep
traceFamily ancestorList sh = foldM (flip ($)) sh ancestorList

--
-- some complex queries using the trace functions
paternalGrandMotherT :: Sheep -> Maybe Sheep
paternalGrandMotherT = traceFamily [father, mother]

maternalGrandMotherT :: Sheep -> Maybe Sheep
maternalGrandMotherT = traceFamily [mother, mother]

paternalGrandFatherT :: Sheep -> Maybe Sheep
paternalGrandFatherT = traceFamily [father, father]

maternalGrandFatherT :: Sheep -> Maybe Sheep
maternalGrandFatherT = traceFamily [mother, father]

mothersPaternalGrandMotherT :: Sheep -> Maybe Sheep
mothersPaternalGrandMotherT = traceFamily [mother, father, mother]

mothersMaternalGrandFatherT :: Sheep -> Maybe Sheep
mothersMaternalGrandFatherT = traceFamily [mother, mother, father]

mothersPaternalGreatGrandFatherT :: Sheep -> Maybe Sheep
mothersPaternalGreatGrandFatherT = traceFamily [mother, father, father, father]

-- testing using the above functions
-- λ> let dolly = sheepFamily
-- λ> paternalGrandMotherT dolly
-- Nothing
-- λ> mothersPaternalGrandMotherT dolly
-- Just (Sheep {name = "Eve", mother = Nothing, father = Nothing})
-- λ> mothersMaternalGrandFatherT dolly
-- Just (Sheep {name = "Adam", mother = Nothing, father = Nothing})
-- λ> mothersPaternalGreatGrandFatherT dolly
-- Just (Sheep {name = "Uranus", mother = Nothing, father = Nothing})

-- filterM function
-- this function works similar to  filter function but in Monadic Context
-- it takes a predicate which returns a boolean value in Monad and a list
-- of values; returns list of values inside Monad which satisfy predicate
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x : xs) = p x >>= \y -> filterM p xs >>=
                        \ys -> return (if y then x : ys else ys)


-- λ> filterM (\x -> Just (x > 0)) [2,1,0,-1]
-- Just [2,1]
-- λ> filterM (\_ -> [True, False]) [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-- There are 2 functions for conditional execution of monadic computations
-- when ; this function takes a boolean and a monadic computation with ()
-- and performs the computation only when boolean argument is true
when :: (Monad m) => Bool -> m () -> m ()
when f x = if f
            then x
            else return ()


-- unless ; does the same as above except that it performs computation
-- unless the boolean argument is false
unless :: (Monad m) => Bool -> m () -> m ()
unless f = when (not f)

-- λ> when (1 == 1) (print "OK")
-- "OK"
-- λ> when (1 == 0) (print "Not OK")
-- λ> unless (1 == 0) (print "Not OK")
-- "Not OK"
-- λ> let simulation = False in unless simulation (putStrLn "Launching Ariane 5 now")
-- Launching Ariane 5 now

-- zipWithM is the monadic version of zipWith over lists
zipWithM :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

-- zipWithM_ works same as zipWithM but ignores the output, which is useful when
-- side effects needs to be considered
zipWithM_ :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

-- ap and lifting functions
-- lifting is an operation to convert non monadic function into an equivalent
-- function that operates on monadic values; essentially it applies a function
-- to a wrapped value and returns a wrapped value similar to fmap
lift :: (Monad m) => (a -> b) -> m a -> m b
lift f xs = xs >>= \y -> return (f y)

-- λ> lift (+2) (Just 3)
-- Just 5

-- higher order lifting
lift2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y = x >>= \z1 -> y >>= \z2 -> return (f z1 z2)

-- create a new operator ?- to subtract numbers encapsulated in monads:
-- λ> let (?-) = lift2 (-) in Just 3 ?- (Just 2)
-- Just 1
--
-- create a new operator ?* to multiply numbers encapsulated in monads,
-- multiply with "Nothing":
-- λ> let (?*) = lift2 (*) in Just 3 ?* Nothing
-- Nothing

-- ap
-- this applies a function inside a monad to a value inside a monad
-- of the same type; a  call to liftN can often be replaced by one
-- or more calls to ap
-- return f `ap` x1 `ap` ... `ap` xn
-- is equivalent to
-- liftN x1 x2 x3 ... xn
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = lift2 ($)

-- λ> [(+ 1), (+ 2)] `ap` [1, 2, 3]
-- [2,3,4,3,4,5]
-- λ> Just (*) `ap` (Just 2) `ap` (Just 3)
-- Just 6

-- conditional execution using guard function
guard :: (MonadPlus m) => Bool -> m ()
guard True  = return ()
guard False = mzero

-- λ> [1..100] >>= \x -> guard ('3' `elem` show x) >> return x
-- [3,13,23,30,31,32,33,34,35,36,37,38,39,43,53,63,73,83,93]

-- join function
join :: (Monad m) => m (m a) -> m a
join x = x >>= id

-- λ> join (Just (Just 2))
-- Just 2

-- return a list containing the result of folding the binary operator through
-- all combinations of elements of given lists
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations _ []       = []
allCombinations f (x : xs) = foldl (liftM2 f) x xs

-----------------------------------------------------------------------------------
-- The Identity Functor, Applicative and Monad
-----------------------------------------------------------------------------------
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
   fmap f x = Identity (f (runIdentity x))

instance Applicative Identity where
   pure = Identity
   Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
   return = Identity
   (Identity x) >>= f = f x

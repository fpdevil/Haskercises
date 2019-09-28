{-|
Author     : Sampath Singamsetty
Maintainer :
File       : MonadEx1.hs
Description: General exercises with Monads in Haskell.
             learning by re-implementing the standard monads
-}

import           Control.Applicative
import           Data.Monoid

-----------------------------------------------------------------------------------
-- Writer Monad
--
-- The Writer monad depends on two types. `a` is the type of the data stored into
-- the monad, and `w` is the type of the additional information (decoration) which
-- is attached to the monad. `w` is the output value accumulated along with the
-- computational result
--
-- Its main principle is that computations will append previously existing data
-- with data provided by the function called by `>>=`. This is the role of `>>=`
-- too take care of appending all decoration data.

newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Show)

-- a constructor for the Writer computation from a (result, output) pair
-- its the inverse of runWriter
writer :: (a, w) -> Writer w a
writer = Writer

-- A writer is a `Functor`: the decoration is untouched by function application.
instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

-- Applicative instance of Writer
instance (Monoid w) => Applicative (Writer w) where
    pure a                            = Writer (a, mempty)
    Writer (f, wf) <*> Writer (a, wa) = Writer (f a, wf `mappend` wa)

-- Monadic instance of Writer
instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    (Writer (x, u)) >>= f = Writer (y, u `mappend` v)
                            where
                                (y, v) = f x


{-
Inside a Writer you can't inspect what has been written, until you run
(or "unwrap") the  monad, using execWriter or  runWriter. However, you
can use  listen to inspect what  some sub-action wrote to  the writer,
before the value is appended to the writer state, and you can use pass
to modify what is written.
-}


-- `tell is the way to add some decoration. Using tell in the context of a Monad
-- `will create a new writer with a content of () (the unit) and the given
-- `decoration. It will then be considered during subsequent bind calls.
tell :: (Monoid w) => w -> Writer w ()
tell x = Writer ((), x)

-- clearing the Writer declaration
clear :: (Monoid w) => Writer w ()
clear = Writer ((), mempty)

-- listen m is an action that executes the action m and adds its output to the
-- value of the computation. It listens to the acting monad and returns what
-- the monad "said"
-- `listen` gives  access to the log  produced by a Writer  action inside
-- the Writer monad. It can be used to inspect what some sub-action wrote
-- to the writer, before the value is appended to the writer state

listen :: (Monoid w) => Writer w a -> Writer w (a, w)
listen x = Writer ((a, w), w)
           where
           (a, w) = runWriter x

-- pass m is an action that executes the action m, which returns a value and a
-- function, and returns the value, applying the function to the output. It
-- provides a writer transformer which can change the internals of the written
-- object.
pass :: (Monoid w) => Writer w (a, w -> w) -> Writer w a
pass x = Writer (a, f w)
         where
         ((a, f), w) = runWriter x

-- Write only the result of the computation, forgetting all about the decoration
compute :: (Monoid w) => Writer w a -> a
compute = fst .runWriter

-- get only the log or the accumulated value from the context
logs :: (Monoid w) => Writer w a -> w
logs = snd . runWriter

-- Extract the output from a writer computation
execWriter :: (Monoid w) => Writer w a -> w
execWriter x = snd (runWriter x)

-- Map both the return value and output of a computation using a given function.
mapWriter :: ((a, w) -> (b, z)) -> Writer w a -> Writer z b
mapWriter f m = Writer $ f (runWriter m)

{-|
   Author       : Sampath Singamsetty
   Maintainer   :
   File         : MonadEx3.hs
   Description  : General exercises using Monads in Haskell
                  learning by re-implementing the State Monaad
-}
module MonadEx3 where

import           Control.Applicative ()
import           Control.Monad       ()
-----------------------------------------------------------------------------------
{-
                                 STATE MONAD
                                 ===========

Two standard programming paradigms common in the computational world
1. Imperative style: Here variables are mutable and they are changed
2. Functional style: Here variables are immutable and hence new variables are
                     created for holding new values.

State Monad enables to build  computations which manipulate state init
the Functional paradigm. The concept of  state refers to a function or
a  computation(s) global  or non-local  state  at any  given point  of
time. Now  when the output  from such a  function/computtation depends
solely on the received input(s), we can say that iterate is STATELESS;
an  example of  which is  the Combinatory  Logic. Conversely  when the
output depends  not only on just  the received input, but  also on the
history of  some or  all previous  executions, we can  say that  it is
STATEFUL; an example for which is the Sequential Logic.

Note  that  the variables  Local  to  some  scope are  not  considered
Stateful as they  die along with their scope and  thus cannot keep the
historical data  of previous  computations. So,  we only  consider the
Global and Non-Local variables to be Stateful.

Concisely, here is how a Stateful computation can be depicted

{Input State} -> (\\ On Some Operation \\)
              .....
              -> {result, New State}

State type enables us to  build computations that manipulate (internal
or hidden) state.  A computation which can take and  initial State (of
type `s`)  and returns a  tuple consiting of  the result value  of the
computation  (of type  `a`)  and a  new  state (of  type  `s`) can  be
expressed as a function of the type `s -> (a, s)`

for instance, `\s -> ("abc", s)`  would be a computation which returns
`"abc"` as the result value with its state unchanged

similarly `\s  -> (s, s+2)` would  be a computation which  returns the
initial state as  the result value along with a  new state incremented
by a value 2
-}
-----------------------------------------------------------------------------------
-- A stateful computation can be represented in a purely-functional way as
-- a function from states to pairs of result and new state:
--
--           f : state -> {result, state}
--
-- if s = type of the State of the computation
--    a = type of the produced result
-- then we can define the State interms of a newtype as follows:
--
newtype State s a = State { runState :: s -> (a, s) }

-- The runState function takes a stateful  computation or a value `a` and
-- an initial  State `s` as  parameters. It unwraps the  computation from
-- the State type and simply applies it to the initial State. In  a sense
-- it is simply the opposite of state.
--
-- with newtype definition, the runState will be as follows
-- runState :: State s a -> s -> (a, s)
--
-- its analgous to runState (State f) s =  f s
--
-- here is how the flow of state can be depicted diagrammatically
--
--                    input
--                    state
--                      ⇓
--                   -------
--                   |     |
--                   |     | ⇒ result
--                   |     |
--                   -------
--                      ⇓
--                    output
--                    state


-----------------------------------------------------------------------------------
-- define a state constructor
-----------------------------------------------------------------------------------
state :: (s -> (a, s)) -> State s a
state = State

-----------------------------------------------------------------------------------
-- a  state monad  is  made an  instance Functor,  which  lets the  state
-- transformation functionality untouched. It can be pictorially depicted
-- with the below diagram.
--
--                      s
--                      ⇓
--                   -------
--                   |     |
--                   |  ma | ⇒ a
--                   |     |
--                   -------
--                      ⇓
--                      t
--
-- then net  effect of mapping a  function isDenormalized to wrap  up the
-- previous  State  transforming function  in  a  new State  transforming
-- function which calls the old State transforming function capturing and
-- adjusting its return value
-----------------------------------------------------------------------------------
instance Functor (State s) where
  fmap f ma = State $ \s -> let (a, t) = runState ma s in (f a, t)

-----------------------------------------------------------------------------------
-- Making State s an instance of Applicative
-- pure should just wrap the value  into a default minimal context, so in
-- this case it produces  a State function that does not  change or use the
-- State s
--
-- The action (<*>)  takes the input state af and  obtains the function f
-- along  with an  updated state  t; this  updated state  t will  then be
-- passed to the function ax obtaining a value a along with a new updated
-- state u. Finally the new value f a and a final state u are returned as
-- a pair. This can be pictorially depicted as below
--
--                      s
--                      ⇓
--                   -------
--                   |     |
--                   |  af | ⇒ f ------\
--                   |     |             \
--                   -------              \
--                      ⇓                  \
--                      t                    \
--                      ⇓     af <*> ax      ----> f x
--                      t                    /
--                      ⇓                  /
--                   -------              /
--                   |     |             /
--                   |  ax | ⇒ x ------/
--                   |     |
--                   -------
--                      ⇓
--                      u
--
-----------------------------------------------------------------------------------
instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  af <*> ax = State $ \s ->
    let (f, t) = runState af s
        (x, u) = runState ax t
    in (f x, u)

-----------------------------------------------------------------------------------
-- State as a Monad
-- Finally we  can make State  an instance of  a Monad. Here  return just
-- wraps  the  value  into  a  default minimal  context  with  the  State
-- untouched,  while bind  (>>=)  combines the  two state  transformation
-- functions. It just the follows convention [ma >>= (a -> mb) -> mb]
-----------------------------------------------------------------------------------
instance Monad (State s) where
  return x = State $ \s -> (x, s)
  mx >>= f = State $ \s ->
    let (x, t) = runState mx s
        my     = f x
        (y, u) = runState my t
    in (y, u)

-----------------------------------------------------------------------------------
-- internal State extraction through the `get` function; it generates a
-- computation that returns the unchaged state as both the result value
-- and the new State
-----------------------------------------------------------------------------------
get :: State s s
get = State $ \s -> (s, s)

-----------------------------------------------------------------------------------
-- Set's or replaces the State inside the Monadic context using the `put`
-- function; it generates a computation that returns whatever state as the
-- result value and as the new state
-----------------------------------------------------------------------------------
put :: s -> State s ()
put x = State $ const ((), x)
-- put x = State $ \s -> ((), x)

-----------------------------------------------------------------------------------
-- The `modify` function modifies the state (read/write/modify). Its a
-- monadic state transformer that maps an old state to a new state within
-- a State monad. The old State is simply thrown away.
-----------------------------------------------------------------------------------
modify :: (s -> s) -> State s ()
modify f = do
    current <- get
    put $ f current

-----------------------------------------------------------------------------------
-- The function `evalState` will evaluate a stateful computation with the
-- given initial state and returns the final value , discarding final State
-- evalState (State f) s = first (f s)
-----------------------------------------------------------------------------------
evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

-----------------------------------------------------------------------------------
-- The function `execState` will evaluate a stateful computation with the
-- given initial state and returns the final state, discarding final value
-- execState (State f) s = second (f s)
-----------------------------------------------------------------------------------
execState :: State s a -> s -> s
execState ma s = snd (runState ma s)

-----------------------------------------------------------------------------------
-- The function `mapState` maps both the return value and the final state
-- of a computation using the given function
-----------------------------------------------------------------------------------
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

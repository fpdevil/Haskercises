{-|
Author     : Sampath Singamsetty
Maintainer :
File       : MonadEx4.hs
Description: General Monad examples and exercises in Haskell
             Using State Monad
-}

module Monads.MonadEx4 where

import           Control.Monad
import           Control.Monad.State
-----------------------------------------------------------------------------------
-- simple state test
tstState :: Int -> (String, Int)
tstState = runState f
           where
           f = get >>= \x -> put (x * 10) >> return ['a'..'z']

-- λ> tstState 6
-- ("abcdefghijklmnopqrstuvwxyz",60)

-- a simple number incrementor
inc :: State Int Int
inc = get >>= \n -> put (2*n + 1) >> return n

incBy :: Int -> State Int Int
incBy n = get >>= \_ -> modify (+ n) >> return n

testinc :: IO ()
testinc = do
  print $ evalState inc 10
  print $ execState inc 1
  print $ runState inc 5
  print $ runState (withState (+ 6) inc) 1
  print $ runState (incBy 4) 6
  print $ runState (mapState (\(a, s) -> (a + 3, s + 7)) inc) 1

-- λ> testinc
-- 10
-- 3
-- (5,11)
-- (7,15)
-- (4,10)
-- (4,10)

-----------------------------------------------------------------------------------
-- fibonacci numbers
fib :: Int -> State Int Int
fib 0 = return 1
fib 1 = return 1
fib n = do
  a <- fib (n - 1)
  b <- fib (n - 2)
  modify (+1)
  return (a + b)

-- λ> runState (fib 10) 0
-- (89,88)

-- generating a stream of unique labels
-- Let us  say we have a  requirement of generating labels  in code while
-- performing operations on an Abstract  Syntax Tree; for this each label
-- must be  unique and we  need labels  in various functions;  since init
-- haskell we cannot mutate the variables directly we have to control the
-- state

-- define a type for holding the State (contains a number)
type Label = State Int

-- increment the internal state and return a Label
incLabel :: Label String
incLabel = state $ \x -> let y = x + 1
                        in ("$" ++ show y, y)

-- generate a pair of labels by lifting the above functions
labelPair :: Label (String, String)
labelPair = (,) <$> incLabel <*> incLabel

-- now define a function for generating the labels
labels :: Bool -> Label [(String, String)]
labels predicate = func <$> labelPair
                        <*> labelPair
                        <*> labelPair
                        where
                          func x y z = if predicate
                                          then [x, z]
                                          else [x, y, z]

-- now run the labels function
runLabels :: IO ()
runLabels = do
  putStrLn "Enter the predicate: `True` or `False`"
  predicate <- getLine
  print $ evalState (labels . read $ predicate) 0

-- λ> runLabels
-- Enter the predicate: `True` or `False`
-- False
-- [("$1","$2"),("$3","$4"),("$5","$6")]
-- λ> runLabels
-- Enter the predicate: `True` or `False`
-- True
-- [("$1","$2"),("$5","$6")]
-----------------------------------------------------------------------------------
-- Calculator using State monad
-- define a type for the calculation for internal usage
--
type CalcState = State [Double]

-- define a type for the exported usage of Calculations
type Calculate = CalcState ()

-- for implementing the Store and Recall functions as in a standard calculator
-- we have make changes toEnum our original type CalcState toEnum also include
-- additional information for memory location
-- creating a record data type for state
data InternalState = InternalState
                         { stack  :: [Double]
                         , memory :: Double
                         }

-- redefining the CalcState...
type CalculatorState = State InternalState

-- define monadic  functions to Push  and Pop values off  the Calculation
-- Stack these can be built on top of the `get` and `put` functions
cpop :: CalcState Double
cpop = do
  stk <- get
  case stk of
    []       -> return 0.0
    (x : xs) -> do
      put xs
      return x

cpush :: Double -> CalcState ()
cpush p = do
  stk <- get
  put (p : stk)

-- new push and pop functions using the InternalState
pop :: CalculatorState Double
pop = state $ \st -> case stack st of
                       []       -> (0.0, st)
                       (x : xs) -> (x, st { stack = xs })

push :: Double -> CalculatorState ()
push p = modify $ \stk -> stk { stack = p : stack stk }

-- enter value for calculation
xEnter :: Double -> CalculatorState ()
xEnter = push

-- binary arithmetic operation
binOp :: (Double -> Double -> Double)-> Calculate
binOp op = do
  y <- cpop
  x <- cpop
  cpush (op x y)

xAdd, xSub, xMul, xDiv :: Calculate
xAdd = binOp (+)
xSub = binOp (-)
xMul = binOp (*)
xDiv = binOp (/)

-- actual calculation of results
apply :: Calculate -> Double
apply mx = fst $ runState (mx >> cpop) []
-- conceptually same as the below
-- apply mx = head . snd $ runState mx []
-- this fails if mx returns an empty list. the previous case handles
-- this by inserting an additional pop which provides default value

-- same function using evalState function
performCalc :: Calculate -> Double
performCalc mx = evalState (mx >> cpop) []

perform :: Calculate -> Double
perform mx = evalState (mx >> pop) startState
    where
        startState = InternalState { stack = [], memory = 0.0 }


-- testing a simple RPN expression
-- (4 5 + 3 *) = 27
test :: Double
test = performCalc $ do
    xEnter 4
    xEnter 5
    xAdd
    xEnter 3
    xMul

-- swap two elements provided
xSwap :: Calculate
xSwap = do
    y <- pop
    x <- pop
    push y
    push x

-- duplicate an element provided
xDup :: CalcState ()
xDup = do
    x <- pop
    push x
    push x

-- unary operation on a value
unOp :: (Double -> Double) -> CalcState ()
unOp op = do
    x <- pop
    push (op x)

-- function forM square root
xSqrt :: Calculate
xSqrt = unOp sqrt

-- trigonometric functions
xSin, xCos, xTan :: Calculate
xSin = unOp sin
xCos = unOp cos
xTan = unOp tan

-- square root function
square :: Calculate
square = do
  xDup
  xMul

-- hypotenuse calculation
hypotenuse :: Calculate
hypotenuse = do
  square
  xSwap
  square
  xAdd
  xSqrt

-- λ> performCalc  $ xEnter 3 >> xEnter 4 >> hypotenuse
-- 5.0

{-|
   Author     : Sampath Singamsetty
   Maintainer :
   File       : MonadEx2.hs
   Description: General exercises with Writer Monad in Haskell.
                learning by re-implementing the standard monads
-}

module MonadEx2
(
  moves,
  showMoves,
  hanoi,
  binpow,
  deleteOn,
  logTwo,
  getUserInput
)
where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Monoid
-----------------------------------------------------------------------------------
-- Towers of Hanoi
-- define the 3 Pegs
data Peg = A
         | B
         | C
         deriving (Show)

-- for a 3 pegged Towers Of Hanoi, we can categorize the pegs as follows
-- Source or Starting Peg         : src
-- Target or Destination Peg      : target
-- Auxilliary or Intermediate Peg : aux

-- function for handling the disk movement across the pegs
moves :: Peg -> Peg -> Peg -> Int -> Writer [String] Int
moves src aux target n
  | n == 1 = writer (1, [show src ++ " => " ++ show target])
  | otherwise = do
      -- first move the top (n - 1) disks from src to aux via target
      x <- moves src target aux (n - 1)
      -- move the remaining n th disk on src to target
      _ <- moves src aux target 1
      -- now move the (n - 1) disks on the aux to target via src
      y <- moves aux src target (n - 1)
      return (1 + x + y)

-- define a function for rendering the results
showMoves :: (Int, [String]) -> IO ()
showMoves (n, steps) = do
    putStrLn "-----------"
    putStrLn $ "Towers Of Hanoi solved with " ++ show n ++ " moves !!!"
    mapM_ print steps
    putStrLn "-----------"

hanoi :: Int -> IO ()
hanoi n = showMoves $ runWriter (moves A B C n)

-- λ> hanoi 3
-- -----------
-- Towers Of Hanoi solved with 7 moves !!!
-- "A => C"
-- "A => B"
-- "C => B"
-- "A => C"
-- "B => A"
-- "B => C"
-- "A => C"
-- -----------

-- power of one number to the other
binpow :: Int -> Int -> Writer [String] Int
binpow _ 0 = return 1
binpow x y
  | even y = binpow x (y `div` 2) >>=
             \z -> writer (z * z, ["Squaring " ++ show z])
  | otherwise = binpow x (y - 1) >>=
                \z -> writer (x * z, ["Multiplying " ++ show x ++ " and " ++ show z])


-- λ> mapM_ print $ execWriter $ binpow 2 3 >> binpow 3 7
-- "Multiplying 2 and 1"
-- "Squaring 2"
-- "Multiplying 2 and 4"
-- "Multiplying 3 and 1"
-- "Squaring 3"
-- "Multiplying 3 and 9"
-- "Squaring 27"
-- "Multiplying 3 and 729"

{-
Inside a Writer you can't inspect what has been written, until you run
(or "unwrap") the  monad, using execWriter or  runWriter. However, you
can use  listen to inspect what  some sub-action wrote to  the writer,
before the value is appended to the writer state, and you can use pass
to modify what is written.

Ex: Log an action only if the log produced by the Writer satisifies a
predicate condition.
-}

deleteOn :: (Monoid w) => (w -> Bool) -> Writer w a -> Writer w a
deleteOn predicate m = pass $ do
  (a, w) <- listen m
  if predicate w
     then return (a, id)
     else return (a, const mempty)

logTwo :: Writer [String] ()
logTwo = do
  deleteOn ((> 5) . length . head) $ tell ["basket"]
  deleteOn ((> 5) . length . head) $ tell ["ball"]
  deleteOn ((== 3) . length . head) $ tell ["pie"]

-- λ> logTwo
-- WriterT (Identity ((),["basket","pie"]))
-- λ> runWriter logTwo
-- ((),["basket","pie"])

{-
Monad  stack for  the application  uses  IO, wrapped  in a  logging
Writer    Using    WriterT    Writer    transformation    as    per
http://stackoverflow.com/questions/7630350/writer-vs-writert-in-haskell
The difference  is that  Writer is  a monad,  whereas WriterT  is a
monad transformer, i.e.  you give it some underlying  monad, and it
gives you  back a new monad  with "writer" features on  top. If you
only need the writer-specific features, use Writer.  If you need to
combine its effects with some other monad, such as IO, use WriterT.
-}

type App a = WriterT [String] IO ()

-- a function toEnum write toEnum log
logMsg :: String -> App ()
logMsg msg = tell [msg]

-- take input from the user and log the same
getUserInput :: App Int
getUserInput = do
  logMsg "get the user data"
  m <- liftIO getLine
  logMsg $ "got line: " ++ show m
  (return . read) m

-- use getUserInput and increment the result by 9
app :: App Int
app = do
  m <- getUserInput
  return (m + 9)

-----------------------------------------------------------------------------------

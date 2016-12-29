{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise12.hs
Description: General exercises in Haskell

-}

module Haskercise12
(
zero,
one,
two,
three,
four,
five,
six,
seven,
eight,
nine,
ten,
nat,
infinity,
even,
odd,
rotChar,
rotStdIn,
usage,
lis2num,
main
)
where


import qualified Data.Char          as C
import           Prelude            hiding (even, odd)
import           System.Environment
import           System.Exit
import           System.IO

-----------------------------------------------------------------------------------
{-
CMSC16100 Peano numbers

- 0 is a natural number
- f is  a one-to-one function  from natural numbers to  natural numbers,
  i.e., for all  natural numbers a, f(a) is also  a natural number; also
  for all natural numbers a and b if f(a) = f(b) then a = b
- for all a, f(a) /= 0 ; 0 is not a successor
- every natural number is either 0 or a successor
-}

data NaturalNumber = Zero | F NaturalNumber deriving (Show)

-- some common natural numbers
zero, one, two, three, four, five, six, seven, eight, nine, ten :: NaturalNumber
zero  = Zero
one   = F zero
two   = F one
three = F two
four  = F three
five  = F four
six   = F five
seven = F six
eight = F seven
nine  = F eight
ten   = F nine

-- making NaturalNumber instance of Eq
instance Eq NaturalNumber where
  Zero == Zero = True
  F x == F y   = x == y
  _ == _       = False


-- instance of Num
instance Num NaturalNumber where
  x + Zero = x
  x + F y  = F (x + y)
  x - Zero = x
  _ * Zero = Zero
  x * F y  = x * y + x
  abs      = undefined
  signum   = undefined
  fromInteger x
    | x > 0     = F (fromIntegral (x - 1))
    | x == 0     = Zero
    | otherwise = Zero


-- representing infinity
infinity :: NaturalNumber
infinity = F infinity

-- function from a natural number to natural number
nat :: NaturalNumber -> NaturalNumber
nat = id

-- λ> two * three + one
-- F (F (F (F (F (F (F Zero))))))
-- λ> nat (2 * 3 + 1)
-- F (F (F (F (F (F (F Zero))))))

-- even numbers
even :: NaturalNumber -> Bool
even x
  | x == 0 = True
  | x == 1 = False
  | otherwise = even (x - 2)

-- odd numbers
odd :: NaturalNumber -> Bool
odd x
  | x == 0 = False
  | x == 1 = True
  | otherwise = odd (x - 1)

-- rot 13 for a single character
rotChar :: Int -> Char -> Char
rotChar n ch
  | C.isLower ch = rotConvert 'a' ch
  | C.isUpper ch = rotConvert 'A' ch
  | otherwise    = ch
  where
    rotConvert :: Char -> Char -> Char
    rotConvert x y = C.chr (C.ord x + (C.ord y - C.ord x + n) `mod` 26)

rotStdIn :: Int -> IO ()
rotStdIn n = do
  input <- getContents
  let output = map (rotChar n) input
  putStr output

-- usage action for handling error conditions
usage :: IO ()
usage = do
  progName <- getProgName
  hPutStrLn stderr $ "usage: " ++ progName ++ " [n]"
  exitWith $ ExitFailure 255

digits :: [Int]
digits = [0 .. 9]

-- cnvert list to a number
lis2num :: [Int] -> Int
lis2num = foldl (\x y -> 10*x + y) 0

-- main method
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> rotStdIn 13
    [x] | all C.isDigit x -> rotStdIn (read x)
        | otherwise       -> usage
    _  -> usage

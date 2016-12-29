{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise18.hs
Description: General examples and exercises in Haskell

-}

module Haskercise18 where

import           Control.Applicative
import           Control.Monad.Cont
import qualified Data.Char                   as C
import qualified Data.List                   as L
import           Data.List.Split
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format
import           Prelude                     hiding (even, odd)
-----------------------------------------------------------------------------------
counter :: IO ()
counter = do
  forM_ [1..3] $ \x -> print x

  forM_ [7..9] $ \y -> print y

  withBreak $ \z ->
    forM_ [1..] $ \_ -> do
      p "loop"
      z ()
  where
    withBreak = (`runContT` return) . callCC
    p = liftIO . putStrLn

-- simple fizzbuzz
fb :: Int -> [String]
fb n = fizzbuzz [1 .. n]

fizzbuzz :: [Int] -> [String]
fizzbuzz = map (\x -> aux (x `rem` 3, x `rem` 5))

aux :: (Int, Int) -> String
aux (0, 0) = "fizzbuzz"
aux (0, _) = "fizz"
aux (_, 0) = "buzz"
aux (_, _) = ""
-----------------------------------------------------------------------------------
-- factors of a number
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `rem` x == 0]

-- check if number is prime
prime :: Int -> Bool
prime n = factors n == [1, n]

-- list of primes
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

-- positions of a value in a list
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (y, i) <- zip xs [0 ..], x == y]

-- number of lower case characters
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- count number of characters in a string
count :: Char -> String -> Int
count _ []       = 0
count c (x : xs) = if x == c then 1 + count c xs else count c xs

let2int :: Char -> Int
let2int c = C.ord c - C.ord 'a'

int2let :: Int -> Char
int2let n = C.chr (C.ord 'a' + n)

rotate :: Int -> Char -> Char
rotate n c
  | C.isLower c = int2let $ (let2int c + n) `mod` 26
  | otherwise   = c

encode :: Int -> String -> String
encode n = map (rotate n)

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

-- frequencies of characters in a string
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

-- insertion sort
isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)
    where
        insert :: (Ord a) => a -> [a] -> [a]
        insert y [] = [y]
        insert y (z : zs)
          | y <= z = y : z : zs
          | otherwise = y : insert z zs

-- mutual recursion
even :: Int -> Bool
even 0 = True
even n = odd (n - 1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n - 1)

-- get elements at even and odd positions
evens :: [a] -> [a]
evens []       = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds []       = []
odds (_ : xs) = evens xs
-----------------------------------------------------------------------------------
timer :: IO ()
timer = do
  now <- getCurrentTime
  print now

  let next = UTCTime { utctDay = fromGregorian 2016 12 24
                     , utctDayTime = timeOfDayToTime (TimeOfDay 19 24 44.123456789)
                     }
  print next

  let (y, m, d) = toGregorian . utctDay $ next
  print y
  print m
  print d
  let hour = todHour . timeToTimeOfDay . utctDayTime $ next
  let minute = todMin . timeToTimeOfDay . utctDayTime $ next
  let (sec, nano) = properFraction . todSec . timeToTimeOfDay . utctDayTime $ next
  let (_, _, wk) = toWeekDate . utctDay $ next
  print hour
  print minute
  print sec
  print nano
  print wk

  print $ next < now
  print $ next > now

-----------------------------------------------------------------------------------
-- Luhn's algorithm for checking bank card numbers
-- each digit is a separate number
-- from left, double each other number from second last
-- subtract 9 from each number now greater than 9
-- add all resulting numbers together
-- if total is divisible by 10, the card number is valid
--
--function doubles a number and subtracts 9 if more than 9
luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9
               else 2 * x

-- handling card numbers of 4 digits
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = z `mod` 10 == 0
                where
                    z = d + luhnDouble c + b + luhnDouble a

-----------------------------------------------------------------------------------
{-
 Implement a calander

Data.Time.Calendar deals  with days in  a calendar, and  arithmetic on
calendar  dates. Data.Time.Clock  deals with  times in  UT1, which  is
(loosely speaking) time  measured by the Earth,  “adjusted for various
wobbles”.Data.Time.Format exposes an API to the standard strftime-like
interface for reading and formatting times and dates.

λ> import Data.Time
λ> import Data.Time.Format
λ> import Data.Time.Clock
λ> import Data.Time.Calendar
λ> now <- getCurrentTime
λ> now
    2016 - 12 - 24 21 : 2 : 46.166604 UTC
λ> :t utctDay
utctDay :: UTCTime -> Day
λ> utctDay now
    2016 - 12 - 24
λ> fromGregorian 2016 12 24
    2016 - 12 - 24
λ> toGregorian $ utctDay now
    (2016, 12, 24)
λ> addDays 1 $ utctDay now
    2016 - 12 - 25
-}
-- get the month number for a given Date
getMonthNumber :: Day -> Int
getMonthNumber d = let (_, mm, _) = toGregorian d
                   in mm

-- get the week number for a given Date
getWeekNumber :: Day -> Int
getWeekNumber d = let (_, w, _) = toWeekDate d
                  in w

-- get the day number for a given Date starting fromEnum Sunday as 0
getDayNumber :: Day -> Int
getDayNumber d = let (_, _, dd) = toWeekDate (addDays 1 d)
                 in dd - 1

-- given a Date get the Month's name
getMonthName :: Day -> String
getMonthName = formatTime defaultTimeLocale "%B"

-- given a list of Dates group the adjacent Days based on the Month
grpByMonth :: [Day] -> [[Day]]
grpByMonth = L.groupBy month
             where
               month d1 d2 = getMonthNumber d1 == getMonthNumber d2

-- given a list of Dates group the adjacent Days based on the Week
grpByWeek :: [Day] -> [[Day]]
grpByWeek = L.groupBy week
            where
              week d1 d2 = getWeekNumber d1 == getWeekNumber d2

--group Dates by both week and month
grpByWkMonth :: [Day] -> [[[Day]]]
grpByWkMonth days = grpByWeek <$> grpByMonth days

-- express Dates in terms of string representation of Day number
exprDays :: [[[Day]]] -> [[[(String, Day)]]]
exprDays = (fmap . fmap . fmap) (\d -> (formatTime defaultTimeLocale "%_d" d, d))

{-
λ> exprDays $ grpByWkMonth  [utctDay name, (fromGregorian 2016 11 24), (fromGregorian 2015 12 26),(fromGregorian 2017 12 26), (fromGregorian 2014 12 25)]
[[[("25",2016-12-25)]],[[("24",2016-11-24)]],[[("26",2015-12-26),("26",2017-12-26),("25",2014-12-25)]]]
-}

-- function for converting each week to string of days of month; first week of
-- the month padded to left and last week padded to right paired with starting
-- date of the week
exprWeeks :: [[[(String, Day)]]] -> [[(String, Day)]]
exprWeeks = (fmap . fmap) (formatWk . unzip)
    where
        leftPad d  = replicate (3 * getDayNumber d) ' '
        rightPad d = replicate (3 * (6 - getDayNumber d)) ' '
        formatWk :: ([String], [Day]) -> (String, Day)
        formatWk (xs, xd) = (leftPad (head xd) ++ unwords xs ++ rightPad (last xd), head xd)

{-
λ> mapM_ print $ exprWeeks $ exprDays $ grpByWkMonth  [utctDay name, (fromGregorian 2016 11 24), (fromGregorian 2016 12 26),(fromGregorian 2016 12 24), (fromGregorian 2016 12 25)]
[("25                  ",2016-12-25)]
[("            24      ",2016-11-24)]
[("   26               ",2016-12-26),("                  24 25                  ",2016-12-24)]
-}

-- function for concatenating weeks to months with the starting day of the month
--
exprMonths :: [[(String, Day)]] -> [(String, Day)]
exprMonths = fmap (formatM . unzip)
    where
        formatM :: ([String], [Day]) -> (String, Day)
        formatM (ms, md) = let weeks = length ms
                               xs = ms ++ replicate (6 - weeks) (replicate 20 ' ')
                           in
                           (L.intercalate "\n" xs, head md)

{-
 λ> exprDays [[[(fromGregorian 2016 12 24), utctDay now, (fromGregorian 2016 06 30)]]]
 [[[("24",2016-12-24),("25",2016-12-25),("30",2016-06-30)]]]
 λ> exprWeeks $ exprDays [[[(fromGregorian 2016 12 24), utctDay now, (fromGregorian 2016 06 30)]]]
 [[("                  24 25 30      ",2016-12-24)]]
 λ> exprMonths $ exprWeeks $ exprDays [[[(fromGregorian 2016 12 24), utctDay now, (fromGregorian 2016 06 30)]]]
 [("                  24 25 30      \n                    \n                    \n                    \n                    \n                    ",2016-12-24)]
-}

-- attach the name of the month for displaying each month
--
showMonth :: [(String, Day)] -> [String]
showMonth = fmap (\(ms, md) -> formatMonth md ++ "\n" ++ ms)
    where
        formatMonth d = let x = getMonthName d
                            l = length x
                            lPad = (21 - l) `div` 2
                            rPad = 20 - lPad - l
                         in
                         replicate lPad ' ' ++ x ++ replicate rPad ' '

{-
λ> showMonth $ exprMonths $ exprWeeks $ exprDays [[[(fromGregorian 2016 12 24), utctDay now, (fromGregorian 2016 06 30)]]]
["      December      \n                  24 25 30      \n                    \n                    \n                    \n                    \n                    "]
-}

alterMonths :: [[String]] -> [[String]]
alterMonths = fmap helper
              where
                helper x = fmap (L.intercalate "   ") $ L.transpose $ fmap L.lines x

-- calendar formatting
formatC :: Integer -> String
formatC year = L.intercalate "\n" $
               fmap (L.intercalate "\n") $
               alterMonths $
               chunksOf 3 $
               take 12 $
               showMonth $
               exprMonths $
               exprWeeks $
               exprDays $
               grpByWkMonth [fromGregorian year 1 1..]

main :: IO ()
main = putStrLn $ formatC 2016

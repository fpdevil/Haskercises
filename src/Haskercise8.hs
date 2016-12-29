{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise8.hs
Description: A simple Morse code implementation in Haskell

1. A dash is equal to three dots.
2. The space between parts of the same letter is equal to one dot.
3. The space between two letters is equal to three dots.
4. The space between two words is equal to seven dots
-}

module Haskercise8
(
swapChars,
morseCodes,
char2morse,
str2morse,
morse2char,
morse2str,
charToMorse,
stringToMorse,
morseToChar,
convertToMorse,
convertFromMorse,
main,
allowedMorse,
allowedChars,
morseGen,
charGen,
prop_test,
mainTest,
parse,
editStr
) where


import           Control.Monad
import qualified Data.Char          as C
import qualified Data.List          as L
import qualified Data.Map           as M
import qualified Data.Traversable   as T
import           System.Environment
import           System.Exit
import           System.IO
import           Test.QuickCheck
-----------------------------------------------------------------------------------

type MorseStr = String

-- A Map consisting of data for Morse Code conversion
morseCodes :: (M.Map Char MorseStr)
morseCodes = M.fromList
   [ ('a', ".-")
   , ('b', "-...")
   , ('c', "-.-.")
   , ('d', "-..")
   , ('e', ".")
   , ('f', "..-.")
   , ('g', "--.")
   , ('h', "....")
   , ('i', "..")
   , ('j', ".---")
   , ('k', "-.-")
   , ('l', ".-..")
   , ('m', "--")
   , ('n', "-.")
   , ('o', "---")
   , ('p', ".--.")
   , ('q', "--.-")
   , ('r', ".-.")
   , ('s', "...")
   , ('t', "-")
   , ('u', "..-")
   , ('v', "...-")
   , ('w', ".--")
   , ('x', "-..-")
   , ('y', "-.--")
   , ('z', "--..")
   , ('=', "-...-")
   , ('?', "..--..")
   , ('/', "-..-.")
   , (',', "--..--")
   , ('.', ".-.-.-")
   , (':', "---...")
   , ('\'', ".----.")
   , ('-', "-....-")
   , ('(', "-.--.")
   , (')', "-.--.-")
   , ('0', "-----")
   , ('1', ".----")
   , ('2', "..---")
   , ('3', "...--")
   , ('4', "....-")
   , ('5', ".....")
   , ('6', "-....")
   , ('7', "--...")
   , ('8', "---..")
   , ('9', "----.")
   , ('@', ".--.-.")]


-- swap the characters in a tuple
swapChars :: M.Map MorseStr Char
swapChars = M.fromList $ map (\(x, y) -> (y, x)) (M.toList morseCodes)

-- get the morse code of a character if present in map
char2morse :: Char -> MorseStr
char2morse c = case M.lookup (C.toLower c) morseCodes of
                  (Just m) -> m ++ " "
                  Nothing  -> if C.isSpace c then "/" else ""

charToMorse :: Char -> Maybe MorseStr
charToMorse c = M.lookup c morseCodes

-- convert String to Morse code
str2morse :: String -> MorseStr
str2morse = concatMap char2morse

stringToMorse :: String -> Maybe [MorseStr]
stringToMorse str = sequence (fmap charToMorse str)

-- get the character from a valid morse code
morse2char :: MorseStr -> Char
morse2char str = case M.lookup str swapChars of
                   (Just c) -> if c == '/' then ' ' else c
                   Nothing  -> '?'

morseToChar :: MorseStr -> Maybe Char
morseToChar m = M.lookup m swapChars

-- get string representation of morse code
morse2str :: MorseStr -> String
morse2str xs = map morse2char (words xs)

-- conversion functions for to and from morse codes
convertToMorse :: IO ()
convertToMorse = forever $ do
  done <- isEOF
  when done exitSuccess
  line <- getLine
  convertLine line
  where
    convertLine l = do
      let morse = stringToMorse l
      case morse of
        (Just str) -> putStrLn (L.unwords str)
        Nothing -> do
          putStrLn $ "ERROR: " ++ l
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  done <- isEOF
  when done exitSuccess
  -- otherwise
  line <- getLine
  convertLine line
    where
      convertLine l = do
        let decoded :: Maybe String
            decoded = T.traverse morseToChar (L.words l)
        case decoded of
          (Just str) -> putStrLn str
          Nothing -> do
            putStrLn $ "ERROR: " ++ l
            exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] -> case arg of
              "from" -> convertFromMorse
              "to"   -> convertToMorse
              _      -> argError
    _ -> argError
    where
      argError = do
        putStrLn "Please specify the first arg as 'from' or 'to' morse"
        exitFailure

-- Quicktest test cases
allowedChars :: String
allowedChars = M.keys morseCodes

allowedMorse :: [MorseStr]
allowedMorse = M.elems morseCodes

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen MorseStr
morseGen = elements allowedMorse

-- property checking. convert to morse code and back
prop_test :: Property
prop_test = forAll charGen $ \c -> (charToMorse c >>= morseToChar) == Just c

mainTest :: IO ()
mainTest = quickCheck prop_test

-----------------------------------------------------------------------------------
-- A Naive Edit distance implementation
-- Edit distance between 2 strings
-- define the data types
data Edit a = Add a        -- add a character
            | Del a        -- delete a character
            | Sub a a      -- substitute one character with another
            deriving (Show)

-- a function for parsing the strings
-- it returns a list of edit operations needed to move from the
-- source string to the target string
parse :: [a] -> [a] -> [Edit a]
parse = loop
    where
        loop (x : xs) (y : ys) = Sub x y : loop xs ys
        loop [] (y : ys)       = Add y : loop [] ys
        loop (x : xs) []       = Del x : loop xs []
        loop [] []             = []

-- given a source string and a set of edit insrtuctions,
-- create the target string
editStr :: (Eq a) => [a] -> [Edit a] -> Maybe [a]
editStr src ed = sequence (f src ed)
    where
        f :: (Eq a) => [a] -> [Edit a] -> [Maybe a]
        f (x : xs) (Sub y1 y2 : ys) = if y1 == x
                                         then Just y2 : f xs ys
                                         else [Nothing]
        f [] (Sub _ _ : _)          = [Nothing]
        f [] (Add y : ys)           = Just y : f [] ys
        f _ (Add _ : _)             = [Nothing]
        f (x : xs) (Del y : ys)     = if y == x
                                         then f xs ys
                                         else [Nothing]
        f [] (Del _ : _)            = [Nothing]
        f _ []                      = [Nothing]

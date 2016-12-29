{-# LANGUAGE OverloadedStrings #-}
{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise19.hs
Description: General examples and exercises in Haskell
             using AESON for handling the Json data

             Some tasks from Programming in Haskell - G Huton
-}

module Haskercise19 where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad
import           Data.Aeson           hiding (decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char            as C
import           Data.List            (sort)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
-----------------------------------------------------------------------------------
-- define a data tyoe for a person and his tastes
data Person = Person
                  { firstName    :: !T.Text
                  , lastName     :: !T.Text
                  , sex          :: Maybe T.Text
                  , age          :: Int
                  , likesCookies :: Bool
                  } deriving (Show)

-- write instances for fromJSON and toJSON for encoding
instance FromJSON Person where
  parseJSON (Object v) = Person <$>
                         v .: "firstName" <*>
                         v .: "lastName" <*>
                         v .:? "sex" <*>
                         v .: "age" <*>
                         v .: "likesCookies"
  parseJSON _          = mzero

-- instance ToJSON Person where
--   toJSON (Person firstName lastName sex age likesCookies) =
--     object [ "firstName" .= firstName
--            , "lastName" .= lastName
--            , "age" .= age
--            , "likesCookies" .= likesCookies
--            ]
--   toEncoding (Person firstName lastName sex age likesCookies) =
--     pairs ("firstName" .= firstName <>
--            "lastName" .= lastName <>
--            "sex" .= sex <>
--            "age" .= age <>
--            "likesCookies" .= likesCookies
--           )

-- specify the location of json data file to read
jsonFile :: FilePath
jsonFile = "data.json"

-- read the json file
getJSON :: IO BL.ByteString
getJSON = BL.readFile jsonFile

-- print the json data
printJson :: IO ()
printJson = do
  -- get json data and decode the same
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
  case d of
    Left err -> putStrLn err
    Right ps -> print ps

-- pretty printing the same
main :: IO ()
main = (T.unpack . TE.decodeUtf8 . BL.toStrict) <$> getJSON >>= putStrLn

-----------------------------------------------------------------------------------
-- binary string transmission
-- simulating the transmission of a string of characters in the low-level
-- form as a list of binary digits
--
type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
    where
        weights = iterate (*2) 1

{-
    using common algebra for simplifying
    for a bit list [a, b, c, d] can be interpreted as an as follows
    (1 * a) + (2 * b) + (4 * c) + (8 * d)
  = a + (2 * b) + (4 * c) + (8 * d)
  = a + 2 * (b + (2 * c) + (4 * d))
  = a + 2 * (b + 2 * (c + (2 * d)))
  = a + 2 * (b + 2 * (c + 2 * (d + 2 * 0)))
-}

-- re writing the binary to integer conversion function using foldr
-- based on the above algebra
binary2int :: [Bit] -> Int
binary2int = foldr (\x y -> x + 2 * y) 0

-- converting integer to binary
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = y : int2bin x
    where
        (x, y) = n `divMod` 2

-- we may ensure that the binary numbers are of length 8 bits rather than any
-- arbitrary length, by defining a function make8, which can either extend or
-- reduce the binary number with appropriate number of 0's
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Encoding
-- transmission by encoding the string to bits
encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . C.ord)

-- λ> encode "Abc"
-- [1,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

-- Decoding
-- for decoding first define a helper function for splitting the binary list
-- into smaller lists of length in the multiples of 8
split8 :: [Bit] -> [[Bit]]
split8 [] = []
split8 bits = x : split8 y
              where
                (x, y) = splitAt 8 bits

-- decoding the bit list to string
decode :: [Bit] -> String
decode = map (C.chr . binary2int) . split8

-- λ> decode [0,0,1,0,0,1,1,0,1,1,1,1,0,1,1,0,1,1,1,0,0,1,1,0]
-- "dog"

-- the final transmission functions
channel :: [Bit] -> [Bit]
channel = id

transmission :: String -> String
transmission = decode . channel . encode

-- λ> transmission "mickey and donald"
-- "mickey and donald"

-- voting algorithms
-- different algorithms for deciding the winner in an election

-- First past the post
-- each person has a single vote and the candidate with the largest number
-- of votes is declared the winner

-- define a function for counting the number of times an element appears in lists
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

-- function for removing the duplicates of a list
remdups :: (Eq a) => [a] -> [a]
remdups []       = []
remdups (x : xs) = x : filter (/= x) (remdups xs)

-- function for the results
result :: (Ord a) => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- remdups vs]

-- λ> result ["apples","bees","tulips","bees","bees","apples"]
-- [(1,"tulips"),(2,"apples"),(3,"bees")]

-- the winner can now be decided based on the second component
-- of the last computed result
winner :: (Ord a) => [a] -> a
winner = snd . last . result

-- λ> winner ["apples","bees","tulips","bees","bees","apples"]
-- "bees"
-----------------------------------------------------------------------------------
-- Alternative vote
-- here each person can vote for as many candidates as possible listing
-- them in the preference order on their ballot
ballots :: [[String]]
ballots = [["tulips", "bees"],
           ["apples", "bees", "tulips"],
           ["tulips", "apples"],
           ["bees"],
           ["apples", "tulips"],
           ["bees", "apples"],
           ["apples", "tulips", "bees"]]

-- remove empty ballots
rmempty :: (Eq a) => [[a]] -> [[a]]
rmempty = filter (/= [])

-- eliminate a given candidate fromChunks each ballot
elim :: (Eq a) => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- define a function for ranking the 1st choice candidates
-- from each ballot index increasing order of number of votes
rank :: (Ord a) => [[a]] -> [a]
rank = map snd . result . map head

-- λ> rank ballots
-- ["bees","tulips","apples"]

-- winner for the alternate vote algorithm
winner' :: (Ord a) => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c]     -> c
               (c : _) -> winner' (elim c bs)

-- λ> winner' ballots
-- "apples"

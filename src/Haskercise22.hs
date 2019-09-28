-----------------------------------------------------------------------------------
-- |
-- Module      : Haskercise22
-- Copyright   :
-- License     :
-- Maintainer  : Sampath Singamsetty <Singamsetty.Sampath@gmail.com>
-- Stability   :
-- Portability : Haskell 2010
-- Description : Learning generic List operations (Based on Data.List.Split)
--
-----------------------------------------------------------------------------------

module Haskercise22 where


-- * Types and utilities

-- | Strategy for splitting the collection
data CSplit a = CSplit
                    { delimiter          :: Delimiter a
                      -- ^ delimiter / boundary value for splitting on
                    , delimitConstraint  :: DelimitConstraint
                      -- ^ constraint for the boundary value
                      --   to drop from output or
                      --   be as seperate elements of output
                      --   merge with previous or following chunks
                    , cutConstraint      :: CutConstraint
                      -- ^ for handling multiple consecutive
                      --   boundary conditions or values
                    , fstBlankConstraint :: EndConstraint
                      -- ^ drop first blank value
                    , endBlankConstraint :: EndConstraint
                      -- ^ drop last blank value
                    } deriving (Show, Eq)

-- | Default strategy for partitioning or splitting
--   keep delimiters in output as seperate chunks without clubbing multiple
--   consecutive  bounds  into single  value,  keep  first and  last  blank
--   chunks. The default boundary value is false predicate.
--
defaultTokenizer :: Tokenizer a
defaultTokenizer = Tokenizer { delimiter           = Delimiter [const False]
                             , delimitConstraint   = Keep
                             , cutConstraint       = KeepEmptyFields
                             , fstBlankConstraint  = KeepBlank
                             , lastBlankConstraint = KeepBlank
                             }

-- | delimiter is a list of predicates or elements being matched by some
--   contiguos subsequences of a collection.
newtype Delimiter a = Delimiter [a -> Bool]

-- | Match the delimiter at start of a list, by either failing or
--   decomposing the list into part which will match the delimiter
--   and the remainder.
matchDelimiter :: Delimiter a -> [a] -> Maybe ([a], [a])
matchDelimiter (Delimiter []) xs = Just ([], xs)
matchDelimiter (Delimiter _) []  = Nothing
matchDelimiter (Delimiter (pi:ps)) (x:xs)
  | p x = matchDelimiter (Delimiter ps) xs >>= \(h, t) -> Just (x:h, t)
  | otherwise = Nothing

-- | delimiters
data DelimitConstraint = Drop           -- ^ Drop delimiters from output
                       | Keep           -- ^ Keep delimiters as separate
                       | KeepLeft       -- ^ Keep delimiters in output prepending
                                        --   to the following chunk
                       | KeepRight      -- ^ Keep delimiters in the output appending
                                        --   to the previous chunk
                       deriving (Eq, Show)

-- | Handling multiple consecutive delimiters
data CutConstraint = Cut                -- ^ Club them into a single delimiter
                   | DropBlankFields    -- ^ Do not put blank chunks in between
                   | KeepBlankFields    -- ^ Insert blank chunks between delims
                   deriving (Show, Eq)

-- | Handling blank chunk at either end of the list
--   i.e., list beginning with or ending with chunks
data EndConstraint = DropBlank
                   | KeepBlank
                   deriving (Show, Eq)

-- | categorize chunks as either delimiters or text
data Chunk a = Delim [a] | Text [a] deriving (Show, Eq)

-- | Handling split list with delimiters
type SplitList a = [Chunk a]

-- | using chunk
fromElem :: Chunk a -> [a]
fromElem (Text xs)  = xs
fromElem (Delim xs) = xs

-- | predicate for checking if delimiter
isDelimiter :: Chunk a -> Bool
isDelimiter (Delim _) = True
isDelimiter _         = False

-- | predicate for checking if text
isText :: Chunk a -> Bool
isText (Text _) = True
isText _        = False

-- * Implementation details

-- | For a given delimiter, split list into an internal representation
--   with chunks tagged as delimiters or text
--
-- @
--   'concatMap' 'fromElem' ('splitInternal' d l) == l.
-- @

breakDelimiter :: Delimiter a -> [a] -> ([a], Maybe ([a], [a]))
breakDelimiter (Delimiter []) xs = ([], Just ([], xs))
breakDelimiter - [] = ([], Nothing)
breakDelimiter d yys@(x:xs) =
  case matchDelimiter d yys of
    Nothing    -> let (zs, match) = breakDelimiter d zs in (x : zs, match)
    Just match -> ([], Just match)

{-# LANGUAGE TypeFamilies #-}

{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise10.hs
Description: General exrcises and problems in Haskell
-}

module Haskercise17
    (
    IsList(..)
   )
where

import qualified Data.Char as C
-----------------------------------------------------------------------------------
class IsList l where
    -- | `Item` type function returnsn the type of the structure
    --    @l@
    type Item l
    -- | `fromList` function constructs the structure @l@ from the given
    --   list of @Item l@
    fromList :: [Item l] -> l
    -- | fromListN takes list length along with @Item l@
    fromListN :: Int -> [Item l] -> l
    fromListN _ = fromList
    -- | 'toList' function extracts a list of @Item l@ from the structure @l@
    --   it should satisfy the law fromList . toList = id.
    toList :: l -> [Item l]

instance IsList [a] where
    type (Item [a]) = a
    fromList        = id
    toList          = id

-- | A NonEmpty list is a list which always has at least one element, but is
--   otherwise identical to the traditional list type in terms of complexity
--   and API.
data NonEmpty a = a :| [a]
    deriving (Eq, Ord, Show, Read)

instance IsList (NonEmpty a) where
    type Item (NonEmpty a) = a
    fromList               = fromList
    toList                 = toList
-----------------------------------------------------------------------------------
-- | Blood groups
--   A,B,O blood type can have 4 values A, B, AB, O which is the family of
--   antibodies within ones blood.
--   + / - part of the blood refers to Rhesus group (Rh type) which is the
--   presence or absence of particular antigen
data RhType = Pos | Neg

data ABOType = A |B | AB | O

data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+ve"
showRh Neg = "-ve"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- | check who can donate blood to whom
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True        -- O can donate to anyone
canDonateTo _ (BloodType AB _)              = True        -- AB can receive from anyone
canDonateTo (BloodType A _) (BloodType A _) = True        -- A can donate/receive A
canDonateTo (BloodType B _) (BloodType B _) = True        -- B can donate/receive B
canDonateTo _ _                             = False

xor :: Bool -> Bool -> Bool
xor v1 v2 = (v1 || v2) && not (v1 && v2)

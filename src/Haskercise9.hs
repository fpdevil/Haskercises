{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise9.hs
Description: Logic Puzzle solving with Haskell
-}

module Haskercise9
 (
 isChildStatementValid,
 isParent2StatementValid,
 isParent1StatementValid,
 solver,
 main
 ) where

import           Control.Monad
-----------------------------------------------------------------------------------
{-
There is a tribe where all  the Male members speak true statements and
Female  members never  speak two  true statements  in a  row, nor  two
untrue statements  in a  row. A  researcher comes  across a  mother, a
father, and their  child. The mother and father speak  English but the
child  does not.  However,  the researcher  asks the  child "Are you a
boy?". The  child responds but  the researcher doesn't  understand the
response and turns to the parents for a translation.

  Parent 1: The child said 'I am a boy'
  Parent 2: The child is a girl. The child lied

What is the sex of parent 1, parent 2, the child, and what sex did the
child say they (parents) were?

2 axioms can be considered here
  (1) A Male does not lie
  (2) A Female will never tell two lies or two truths in a row

3 statements (logical expressions) can be considered as follows

  (i)  child said a single statement, over which their sex was declared
  (ii) parent-1 said a single statement
       "The child said 'I am a a boy."
  (iii) parent-2 said two statements
       "The child is a girl. The child lied."

Inorder to test the logical validity of the statements wrt axioms, each
of the 3 statemsnts can be realized as a function
-}

-- | declare data type for determining the Sex
data Sex = Male | Female deriving (Show, Eq)

-- | Solution to the puzzle can be in a record form
data Puzzle = Puzzle { parent1           :: Sex
                     , parent2           :: Sex
                     , child             :: Sex
                     , child_description :: Sex
                     }

-- | make Puzzle an instance of Show so that results are rendered
instance Show Puzzle where
  show p = "Parent1 is " ++ show (parent1 p) ++ "\n" ++
           "Parent2 is " ++ show (parent2 p) ++ "\n" ++
           "The child is " ++ show (child p) ++ "\n" ++
           "The child said they both were " ++ show (child_description p) ++ "\n"

{-
verify the statement of child; the function takes below 2 parameters
(the child sex) and the (child described sex)

if (Male, Male) and (Female, *) then they are both valid statements as
if  child is  Male  he will  always  speak truth  and  if Female,  his
response does not matter.

if (Male, Female) then child being  a male is lying which violates the
first axiom and hence not valid.
-}
isChildStatementValid :: Sex -> Sex -> Bool
isChildStatementValid Male Female = False
isChildStatementValid _ _         = True

{-
verify the statement of parent 1; the function takes below paramaters
(parent1 sex) and the (child described sex)
Here, parent 1 said a statement "The child said 'I am a boy'".

Based on  this, if the  parameters are (Male,  Male) then its  a valid
condition as parent being Male does not lie nor the child.

if (Female, *)  then the statement is always valid  as Females have no
guarantee that they are telling truth always.

if (Male,  Female) then its  not a valid  statement as parent  1 being
Male does not lie and says the child  is a boy and Male boy should not
lie and say that he is a Female. This violates our axiom.
-}
isParent1StatementValid :: Sex -> Sex -> Bool
isParent1StatementValid Male Female = False
isParent1StatementValid _ _         = True

{-
verify statement of parent 2; the function takes the below parameters
(parent2 sex), (child sex) and (child described sex)

Here, parent 2 said two statements "The child is a girl. The child lied."

Based on  this, if parent 2  is a Male  the only way the  statement is
valid if the child  is a Female but said that he is  a Male; i.e., the
child infact  lied and is  a Female. Hence  (Male, Female, Male)  is a
valid statement.

if parent 2 is a Female then  it does not matter whether the child sex
is Male or Female as the Female parent(s) statements are not trustful;
hence (Female, *, Female). Hence  from a Female parents(s) perspective
the statement (Female, *, Female) is valid.

rest of the statements are all invalid.
-}
isParent2StatementValid :: Sex -> Sex -> Sex -> Bool
isParent2StatementValid Male Female Male = True
isParent2StatementValid Female _ Female  = True
isParent2StatementValid _ _ _            = False

{-
using lists consisting of [Male, Female] to represent non determinism;
based on the conditions cited above, altogether there are 2^4 possible
solutions or outcomes.
input set(s) with List Monad(s) for each case:
    sex of parent 1        = [Male, Female]
    sex of parent 2        = [Male, Female]
    sex of child           = [Male, Female]
    sex described by child = [Male, Female]

giving overall  2^4 =  16 possible outcomes;  using guard  function of
Monad library  we can filter  out the invalid  cases to arrive  at the
correct solution;  we also need to  consider the special case  of both
parents being of same sex (either gay or lesbian)
-}
solver :: (Sex -> Sex -> Bool) -> [Puzzle]
solver sexuality_predicate = do
  par1    <- [Male, Female]
  par2    <- [Male, Female]
  ch      <- [Male, Female]
  chdescr <- [Male, Female]
  guard $ sexuality_predicate par1 par2
  guard $ isChildStatementValid ch chdescr
  guard $ isParent1StatementValid par1 chdescr
  guard $ isParent2StatementValid par2 ch chdescr
  return $ Puzzle { parent1           = par1
                  , parent2           = par2
                  , child             = ch
                  , child_description = chdescr}

-- | main method for printing the results
main :: IO ()
main = do
  putStrLn "//// Solution for Heterosexual Couples ////"
  mapM_ print $ solver (/=)
  putStrLn "//// Solution for Gay Couples ////"
  mapM_ print $ solver (\x y -> x == y && x == Male)
  putStrLn "//// Solution for Lesbian Couples ///"
  mapM_ print $ solver (\x y -> x == y && x == Female)

-- λ> main
-- //// Solution for Heterosexual Couples ////
-- Parent1 is Female
-- Parent2 is Male
-- The child is Female
-- The child said they both were Male

-- //// Solution for Gay Couples ////
-- Parent1 is Male
-- Parent2 is Male
-- The child is Female
-- The child said they both were Male

-- //// Solution for Lesbian Couples ///
-- Parent1 is Female
-- Parent2 is Female
-- The child is Female
-- The child said they both were Female

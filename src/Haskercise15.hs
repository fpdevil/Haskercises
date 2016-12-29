{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise15.hs
Description: Logic Puzzle Solving exercises in Haskell.
             Solving the Einstein's Puzzle
-}

module Haskercise15 where

import qualified Data.List as L
-----------------------------------------------------------------------------------
{-
From http://www.coudal.com/thefish.php

                         EINSTEIN'S PUZZLE

There are  five houses  in a  row in different  colors. In  each house
lives a person  with a different nationality. The five  owners drink a
different drink, smoke a different brand of cigar and keep a different
pet, one of which is a Walleye Pike.

The question is who owns the fish?

Hints:
1. The Brit lives in the red house.
2. The Swede keeps dogs as pets.
3. The Dane drinks tea.
4. The green house is on the left of the white house.
5. The green house owner drinks coffee.
6. The person who smokes Pall Malls keeps birds.
7. The owner of the yellow house smokes Dunhills.
8. The man living in the house right in the center drinks milk.
9. The man who smokes Blends lives next to the one who keeps cats.
10. The Norwegian lives in the first house.
11. The man who keeps horses lives next to the one who smokes Dunhills.
12. The owner who smokes Bluemasters drinks beer.
13. The German smokes Princes.
14. The Norwegian lives next to the blue house.
15. The man who smokes Blends has a neighbor who drinks water.

There are no  tricks, pure logic will get you  the correct answer. And
yes, there is enough information to arrive at the one and only correct
answer.

This problem has  a factual solution as we were  given clues about the
the habits  and the positions  of the  people which are  not changing;
which means there is no state  change involved any where which implies
we cannot arrive at a possible combination using backtracking. we have
to solve matching the constraints and eliminating the invalid cases
-}

-- there are all together 5 houses and those are the ones included as
-- a part of the result once everything is found out.
-- represent the result set as its own data type
data ResultSet = ResultSet House House House House House
                 deriving (Show, Eq)

-- Represent each house as its own data type
data House = House
                 { color       :: Color
                 , nationality :: Nationality
                 , pet         :: Pet
                 , drink       :: Drink
                 , smoke       :: Smoke
                 } deriving (Show, Eq)


-- explicit House instances of Show and Eq
-- instance Show House where
--     show h = show (nationality h) ++ " lives in a " ++
--              show (color h) ++ " color house, " ++
--              "has a pet as " ++ show (pet h) ++ ", " ++
--              "drinks " ++ show (drink h) ++ " and " ++
--              "smokes " ++ show (smoke h) ++ ". "

-- make House and instance of Eq which will help init equality checks
-- instance Eq House where
--   (House clr1 nat1 pet1 drk1 smk1) == (House clr2 nat2 pet2 drk2 smk2) = clr1 == clr2 &&
--                                                                         nat1 == nat2 &&
--                                                                         pet1 == pet2 &&
--                                                                         drk1 == drk2 &&
--                                                                         smk1 == smk2

-- represent 5 colors of the houses
data Color = Yellow | Blue | Green | White | Red
             deriving (Eq, Show, Enum, Bounded)

-- represent Nationality of each person
data Nationality = British | Swedish | Danish | German | Norwegian
                   deriving (Eq, Show, Enum, Bounded)

-- represent the 5 pets
data Pet = Dog | Cat | Fish | Horse | Birds
           deriving (Show, Eq, Enum, Bounded)

-- represent the 5 drinks they take
data Drink = Tea | Coffee | Beer | Milk | Water
             deriving (Show, Eq, Enum, Bounded)

-- represent what they smoke
data Smoke = PallMalls | Blends | DunHills | Bluemasters | Princes
             deriving (Show, Eq, Enum, Bounded)

-- return list of possible values for each of the data types
colorList :: [Color]
colorList = [Yellow, Blue, Green, White, Red]

nationList :: [Nationality]
nationList = [British, Swedish, Danish, German, Norwegian]

petList :: [Pet]
petList = [Dog, Cat, Fish, Horse, Birds]

drinkList :: [Drink]
drinkList = [Tea, Coffee, Beer, Milk, Water]

smokeList :: [Smoke]
smokeList = [PallMalls, Blends, DunHills, Bluemasters, Princes]
-----------------------------------------------------------------------------------

-- helper functions
-- logical operations to be used for eliminating the cases
{-
xor returns true if both inputs are opposite

A  B  | R
----------
0  0  | 0
1  0  | 1
0  1  | 1
1  1  | 0
-}
xor :: Bool -> Bool -> Bool
xor x y
  | x && not y   = True
  | not x && y   = True
  | otherwise = False

{-
xnor returns true if both inputs are same

A  B  | R
----------
0  0  | 1
1  0  | 0
0  1  | 0
1  1  | 1
-}
xnor :: Bool -> Bool -> Bool
xnor = (not .) . xor

-----------------------------------------------------------------------------------
-- Approach -1
-- define individual constraints based on each clue
-- the preoperties for each house are distinct in the sense that none of the
-- properties in any of the two houses should repeat
distinctConstraint :: ResultSet -> Bool
distinctConstraint (ResultSet h1 h2 h3 h4 h5) = all p hs
                                                  where
                                                    p (House x1 x2 x3 x4 x5, House y1 y2 y3 y4 y5) =
                                                      and [x1 /= y1, x2 /= y2, x3 /= y3, x4 /= y4, x5 /= y5]
                                                    hs =
                                                      [(h1, h2), (h1, h3), (h1, h4), (h1, h5),
                                                       (h2, h3), (h2, h4), (h2, h5),
                                                       (h3, h4), (h3, h5)
                                                      ]

{-
There were 15 clues  which were provided and based on  the type of the
clue, they can  be applied either at an individual  House level or can
be applied at a group of  Houses or the ResultSet. The final filtering
for the actual  solutions has to be  done based on these  15 clues and
constraints

Categories of constraints
           House Level Constraint
           ResultSet Level Constraint

for all the combined clues, if we want both clues to be True at the
same time we can use XNOR
-}

-- clue 1: The Brit lives in the red house
-- House Level Constraint
-- this is True if either the nationality is British and the color is RED
-- are both  simulateneously True or  simultaneously False. For  all such
-- cases we can use the XNOR defined
constraint01 :: House -> Bool
constraint01 h = (nationality h == British) `xnor` (color h == Red)

-- clue 2: The Swede keeps dogs as pets
-- House Level Constraint
constraint02 :: House -> Bool
constraint02 h = (nationality h == Swedish) `xnor` (pet h == Dog)

-- clue 3: The Dane drinks tea
-- House Level Constraint
constraint03 :: House -> Bool
constraint03 h = (nationality h == Danish) `xnor` (drink h == Tea)

-- clue 4: The green house is on the left of the white house
-- ResultSet Level Constraint
-- according to this  clue, its not about an individual  person in a house;
-- this clue can match  any of the two houses in a row  and hence we have
-- to consider the complete result set of 5 houses
constraint04 :: ResultSet -> Bool
constraint04 (ResultSet h1 h2 h3 h4 h5) = any p hs
    where
        p (leftHouse, rightHouse) = color leftHouse == Green && color rightHouse == White
        hs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

-- clue 5: The green house owner drinks coffee
-- House Level Constraint
constraint05 :: House -> Bool
constraint05 h = (color h == Green) `xnor` (drink h == Coffee)

-- clue 6: The person who smokes Pall Malls keeps birds
-- House Level Constraint
constraint06 :: House -> Bool
constraint06 h = (smoke h == PallMalls) `xnor` (pet h == Birds)

-- clue 7: The owner of the yellow house smokes Dunhills
-- House Level Constraint
constraint07 :: House -> Bool
constraint07 h = (color h == Yellow) `xnor` (smoke h == DunHills)

-- clue 8: The man living in the house right in the center drinks milk
-- ResultSet Level Constraint
-- any of the 5  persons can live in the center house  and sortOn we have
-- to consider all the 5 houses as  a group or init otherwords we have to
-- consider te ResultSet
constraint08 :: ResultSet -> Bool
constraint08 (ResultSet _ _ h3 _ _) = drink h3 == Milk

-- clue 9: The man who smokes Blends lives next to the one who keeps cats
-- ResultSet Level Constraint
-- we have to  consider all the 5  houses here as every person  has to be
-- checked to  match the two  neighbours with the specified  habits; also
-- the two  neighbours can be  either left or  right to one  antoherso we
-- have to consider that case as well
constraint09 :: ResultSet -> Bool
constraint09 (ResultSet h1 h2 h3 h4 h5) = any p hs
    where
        p (leftHouse, rightHouse) =
            ((smoke leftHouse == Blends) `xnor` (pet rightHouse == Cat)) ||
            ((smoke rightHouse == Blends) `xnor` (pet leftHouse == Cat))
        hs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

-- clue 10: The Norwegian lives in the first house
-- ResultSet Level Constraint
-- any of the 5 persons can be a Norweegian, so considering all persons
constraint10 :: ResultSet -> Bool
constraint10 (ResultSet h1 _ _ _ _) = nationality h1 == Norwegian

-- clue 11: The man who keeps horses lives next to the one who smokes Dunhills
-- ResultSet Level Constraint
-- again here its not clear about  the positions of the referred persons in
-- the clue and also they can be  either left or right to each other(s) ;
-- hence all the 5 houses or persons have to be checked
constraint11 :: ResultSet -> Bool
constraint11 (ResultSet h1 h2 h3 h4 h5) = any p hs
    where
        p (leftHouse, rightHouse) =
            ((smoke leftHouse == DunHills) `xnor` (pet rightHouse == Horse)) ||
            ((smoke rightHouse == DunHills) `xnor` (pet leftHouse == Horse))
        hs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

-- clue 12: The owner who smokes Bluemasters drinks beer
-- House Level Constraint
-- here the clue is about a particular house owner
constraint12 :: House -> Bool
constraint12 h = (smoke h == Bluemasters) `xnor` (drink h == Beer)

-- clue 13: The German smokes Princes
-- House Level Constraint
-- here the clue is about a particular house owner
constraint13 :: House -> Bool
constraint13 h = (nationality h == German) `xnor` (smoke h == Princes)

-- clue 14: The Norwegian lives next to the blue house
-- Result Level Constraint
-- again, here we  are not sure from the clue  about which particular house
-- owners are having the habits or  properties and also the Norwegian can
-- either be  left or right  to the blue color  house. and hence  all the
-- houses have to be checked
constraint14 :: ResultSet -> Bool
constraint14 (ResultSet h1 h2 h3 h4 h5) = any p hs
    where
        p (leftHouse, rightHouse) =
            ((nationality leftHouse == Norwegian) `xnor` (color rightHouse == Blue)) ||
            ((nationality rightHouse == Norwegian) `xnor` (color leftHouse == Blue))
        hs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]

-- clue 15: The man who smokes Blends has a neighbor who drinks water
-- ResultSet Level Constraint
-- again from the clue, we cannot figure out the persons and also the two
-- being neighbors  can be either left  or right to other;  and sortOn we
-- have toEnum consider all the five houses here
constraint15 :: ResultSet -> Bool
constraint15 (ResultSet h1 h2 h3 h4 h5) = any p hs
    where
        p (leftHouse, rightHouse) =
            ((smoke leftHouse == Blends) `xnor` (drink rightHouse == Water)) ||
            ((smoke rightHouse == Blends) `xnor` (drink leftHouse == Water))
        hs = [(h1, h2), (h2, h3), (h3, h4), (h4, h5)]


-- list all the constraints which can be applied at the House Level
houseLevelConstraints :: [House -> Bool]
houseLevelConstraints = [constraint01, constraint02,
                         constraint03, constraint05,
                         constraint06, constraint07,
                         constraint12, constraint13]

-- list all the constraints which can be applied at the ResultSet Level
-- here we can place the ones more assertive first in the order
resultSetLevelConstraints :: [ResultSet -> Bool]
resultSetLevelConstraints = [constraint08, constraint10,
                             distinctConstraint,
                             constraint04, constraint09,
                             constraint11, constraint14,
                             constraint15
                             ]


-- define another function for applying all the previously defined constraints
applyConstraints :: [a -> Bool] -> a -> Bool
applyConstraints cs h = all ($ h) cs


-- from all the 14 constraints we can observe that we have 5 conditions each
-- having 5 possibilities giving a total of 5⁵ = 3125 combinations. With that
-- the total permutations of size is 3125⁵ which is an arbitrarily long value
-- all combinations of houses
houseCombinations :: [House]
houseCombinations = [House cl nt pt dk sk |
                     cl <- colorList,
                     nt <- nationList,
                     pt <- petList,
                     dk <- drinkList,
                     sk <- smokeList
                    ]


-- get a list of all the houses satisfying the constraints defined above by
-- filtering the list on House Level Constraints
houses :: [House]
houses = filter (applyConstraints houseLevelConstraints) houseCombinations


-- all possible combinations of houses in the ResultSet
resultSetCombinations :: [ResultSet]
resultSetCombinations = [ResultSet h1 h2 h3 h4 h5 |
                         h1 <- hs,
                         h2 <- hs,
                         h3 <- hs,
                         h4 <- hs,
                         h5 <- hs
                        ]
                        where
                          hs = houses

-- get a list of all the result sets satisfying the constraints specific to
-- the ResultSet Level Constraints using the applyConstraints
-- this took loooooooooooooong time and never finished...
results :: [ResultSet]
results = filter (applyConstraints resultSetLevelConstraints) resultSetCombinations
-----------------------------------------------------------------------------------
-- Approach -2
-- re working on it as the earlier results never completed (probably a Bug)
--
-- check all the assertive clues; that is for which the data has been
-- described directly; there are some other clues which are relative,
-- and they will be checked next
-- first checking all the assertive constraints using XNOR
constraints :: [House -> Bool]
constraints = [\hc -> (nationality hc == British)     `xnor` (color hc       == Red),         -- clue 1
               \hc -> (nationality hc == Swedish)     `xnor` (pet hc         == Dog),         -- clue 2
               \hc -> (nationality hc == Danish)      `xnor` (drink hc       == Tea),         -- clue 3
               \hc -> (color hc       == Green)       `xnor` (drink hc       == Coffee),      -- clue 5
               \hc -> (smoke hc       == PallMalls)   `xnor` (pet hc         == Birds),       -- clue 6
               \hc -> (color hc       == Yellow)      `xnor` (smoke hc       == DunHills),    -- clue 7
               \hc -> (smoke hc       == Bluemasters) `xnor` (drink hc       == Beer),        -- clue 12
               \hc -> (nationality hc == German)      `xnor` (smoke hc       == Princes) ]    -- clue 13


-- check the constraints as defined above to the House data structure
-- we will use the applyConstraints defined earlier instead of this
checkConstraints :: [House -> Bool] -> House -> Bool
checkConstraints [] _       = False
checkConstraints [cs] h     = cs h
checkConstraints (c : cs) h = c h && checkConstraints cs h


-- for testing the relative clues or constraints
-- first define a function to test if one person is a Neighbour to another
isNeighbourOf :: (House -> Bool) -> (House -> Bool) -> [House] -> Bool
isNeighbourOf x y (f : g : hs) = ((x f || y f) && (x g || y g)) || isNeighbourOf x y hs
isNeighbourOf _ _ _ = False

-- a function for checking who does what in each house
does :: (Eq a) => (House -> a) -> a -> House -> Bool
does fun x y = fun y == x

-- cover the rest of the constraints which are dependent on the
-- 5 house group as whole instead of a single house and also are neighbours
combinationConstraints :: [House] -> Bool
combinationConstraints hs = [Green, White] `L.isInfixOf` map color hs &&                         -- clue 4
                            isNeighbourOf (does smoke Blends) (does pet Cat) hs &&               -- clue 9
                            isNeighbourOf (does pet Horse) (does smoke DunHills) hs &&           -- clue 11
                            isNeighbourOf (does nationality Norwegian) (does color Blue) hs &&   -- clue 14
                            isNeighbourOf (does smoke Blends) (does drink Water) hs             -- clue 15

-- there are some additional clues as below
clue8 :: House -> Bool
clue8 h = drink h == Milk

clue10 :: House -> Bool
clue10 h = nationality h == Norwegian

clue14 :: House -> Bool
clue14 h = color h == Blue

-- houses satisfying constraints
houseList :: [House]
houseList = filter (applyConstraints constraints) houseCombinations

-- solution set satisfying only the above 3 constraints and combination constraints
-- also inorder to have a unique solution set we will apply the distinct result set
-- constraint defined at the beginning
solutionSet :: [[House]]
solutionSet = [[h1, h2, h3, h4, h5] |
                h1 <- filter (applyConstraints [not . clue8, clue10, not . clue14]) houseList,
                h2 <- filter (applyConstraints [not . clue8, not . clue10, clue14]) houseList,
                h3 <- filter (applyConstraints [clue8, not . clue10, not . clue14]) houseList,
                h4 <- filter (applyConstraints [not . clue8, not . clue10, not . clue14]) houseList,
                h5 <- filter (applyConstraints [not . clue8, not . clue10, not . clue14]) houseList,
                let hs = [h1, h2, h3, h4, h5],
                distinctConstraint (ResultSet h1 h2 h3 h4 h5),
                combinationConstraints hs ]


-- λ> mapM_ print $ head solutionSet
-- House {color = Yellow, nationality = Norwegian, pet = Cat, drink = Water, smoke = DunHills}
-- House {color = Blue, nationality = Danish, pet = Horse, drink = Tea, smoke = Blends}
-- House {color = Red, nationality = British, pet = Birds, drink = Milk, smoke = PallMalls}
-- House {color = Green, nationality = German, pet = Fish, drink = Coffee, smoke = Princes}
-- House {color = White, nationality = Swedish, pet = Dog, drink = Beer, smoke = Bluemasters}

-----------------------------------------------------------------------------------
main :: IO ()
main = mapM_ print $ zip [1 ..] (head solutionSet)

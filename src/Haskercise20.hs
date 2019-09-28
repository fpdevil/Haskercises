{-|
Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise20.hs
Description: General examples and exercises in Haskell
             Some tasks from Programming in Haskell - G Hutton
-}

module Haskercise20 () where

-----------------------------------------------------------------------------------
-- Solving the Countdown problem
-- given a sequence of source numbers and a single target number, attempt
-- to construct an arithmetic expression using each of the source numbers
-- at most once, and such that the result of evaluating the expression is
-- the target number

-- define a data type for the binary operator
data Op = Add
        | Sub
        | Mul
        | Div

-- make Op an instance of Show for rendering purpose
instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "

-- check if  the operation can  be applied to 2  integers in a  valid way
-- inorder to avoid redundant calculations  we have to consider the cases
-- of commutativity and operations with identity element(s)
-- a + b = b + a
-- a * b = b * a
-- a * 1 = a
-- b * 1 = b
-- a / 1 = a
-- b / 1 = b
isValid :: Op -> Int -> Int -> Bool
isValid Add x y = x <= y
isValid Sub x y = x > y
isValid Mul x y = x /= 1 && y /= 1 && x <= y
isValid Div x y = y /= 1 && x `mod` y == 0

-- apply the binary operator to 2 numbers
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- data type declaration  for an expression an expression can  be just an
-- integer or a binary operation performed over 2 integers.
data Expression = Val Int
                | Do Op Expression Expression

-- make Expression an instance of Show
instance Show Expression where
  show (Val n)     = show n
  show (Do op l r) = disp l ++ show op ++ disp r
                     where
                       disp (Val x) = show x
                       disp y       = "{ " ++ show y ++ " }"

-- function for representing an expression as integer list
values :: Expression -> [Int]
values (Val n)    = [n]
values (Do _ l r) = values l ++ values r

-- function  for  calculating  and  returning the  overall  value  of  an
-- expression, provided the value is a positive number
evaluate :: Expression -> [Int]
evaluate (Val n) = [n | n > 0]
evaluate (Do op l r) = [apply op x y | x <- evaluate l
                                     , y <- evaluate r
                                     , isValid op x y ]

{-
λ> values (Do Sub (Do Sub (Val 10) (Val 3))  (Do Div (Val 12) (Val 3)))
[10,3,12,3]
λ> evaluate (Do Sub (Do Sub (Val 10) (Val 3))  (Do Div (Val 12) (Val 3)))
[3]
-}

-- remove element in a list
sublist :: (Eq a) => [a] -> a -> [a]
sublist [] _ = []
sublist (x : xs) y
  | x == y = xs
  | otherwise = x : sublist xs y

-- subsets of a list of values
subsets :: [a] -> [[a]]
subsets []       = [[]]
subsets (x : xs) = [x : y | y <- ys] ++ ys
    where
        ys = subsets xs

-- helper functions for getting the combinations of numbers
-- function for getting all the subsequences of a list
-- same as above, but using map instead of list comprehension
subsequences :: [a] -> [[a]]
subsequences []       = [[]]
subsequences (x : xs) = ys ++ map (x :) ys
                        where
                          ys = subsequences xs

-- pair selections from a list of values
selections :: [a] -> [(a, [a])]
selections []       = []
selections (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- selections xs]

-- all possible ways of inserting an element into a list
interleave :: a -> [a] -> [[a]]
interleave x []         = [[x]]
interleave x z@(y : ys) = (x : z) : map (y :) (interleave x ys)

-- permutations of a list of elements
perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

-- alternative definition to permutations of lists
permutations :: (Eq a) => [a] -> [[a]]
permutations [] = [[]]
permutations z  = [x : ys | x <- z, ys <- permutations (sublist z x)]


{-
  λ> mapM_ print $ perms "123"
  "123"
  "213"
  "231"
  "132"
  "312"
  "321"
  λ> subsequences "123"
  ["","3","2","23","1","13","12","123"]
  λ> mapM_ print $ selections "123"
  ('1',"23")
  ('2',"13")
  ('3',"12")
  λ> mapM_ print $ interleave '1' "23"
  "123"
  "213"
  "231"
-}

-- return a list of combinations of all elements of a list
choices :: [a] -> [[a]]
choices xs = concatMap perms (subsequences xs)

-- λ> choices "abc"
-- ["","c","b","bc","cb","a","ac","ca","ab","ba","abc","bac","bca","acb","cab","cba"]

-- function given an expression, a list of integers and a target number
-- check if the target number can be reached, with expressions
solver :: Expression -> [Int] -> Int -> Bool
solver ex xs n = values ex `elem` choices xs &&
                 evaluate ex == [n]

-- split a list with all possible combinations
split :: [a] -> [([a], [a])]
split []       = []
split [_]      = []
split (x : xs) = ([x], xs) : [(x : ys, zs) | (ys, zs) <- split xs]

-- combine a pair of expressions using the four
-- available numeric operators
combine :: Expression -> Expression -> [Expression]
combine x y = [Do op x y | op <- [Add, Sub, Mul, Div]]


-- given a list of values, get all the possible expressions
-- which can have among the values
exprs :: [Int] -> [Expression]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [ex | (xs, ys) <- split ns
               , x <- exprs xs
               , y <- exprs ys
               , ex <- combine x y]


solutions :: [Int] -> Int -> [Expression]
solutions ns n = [ex | xs <- choices ns
                     , ex <- exprs xs
                     , evaluate ex == [n]]

{-
  λ> mapM_ print $ solutions [1,2,3] 4
  1 + 3
  3 + 1
  { 2 - 1 } + 3
  2 + { 3 - 1 }
  2 * { 3 - 1 }
  { 2 + 3 } - 1
  { 3 - 1 } + 2
  { 3 - 1 } * 2
  3 + { 2 - 1 }
  { 3 + 2 } - 1
-}

-- eliminating the failed expressions so that they will not
-- be included in the results and computations.
type Result = (Expression, Int)

-- define a function for combining a pair of Results toEnum produce
-- a list of possible Results
combineEx :: Result -> Result -> [Result]
combineEx (lx, m) (ly, n) = [(Do op lx ly, apply op m n) | op <- [Add, Sub, Mul, Div]
                                                         , isValid op m n ]

-- function to return all possible results comprising of expressions
-- whose list of values is exactly the given list
results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [rslt | (lx, ry) <- split ns
                   , x         <- results lx
                   , y         <- results ry
                   , rslt      <- combineEx x y ]

solutionsEx :: [Int] -> Int -> [Expression]
solutionsEx ns n = [expr | xs        <- choices ns
                         , (expr, m) <- results xs
                         , m == n]

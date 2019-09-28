{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise4.hs
Description: Countdown problem solved in Haskell
-}

module Haskercise4 () where

import           System.Environment

-----------------------------------------------------------------------------------
-- | Countdown problem
-- The essence of  the problem is as follows: given  a sequence of source
-- numbers and a single target number, attempt to construct an arithmetic
-- expression using  each of the  source numbers  at most once,  and such
-- that the result of evaluating the expression is the target number. The
-- given numbers  are restricted to  being non-zero naturals, as  are the
-- intermediate results  during evaluation  of the expression,  which can
-- otherwise   be  freely   constructed   using  addition,   subtraction,
-- multiplication and division.

-- | define a binary operator
data Op = Add | Sub | Mul | Div

instance Show Op where
   show Add = " + "
   show Sub = " - "
   show Mul = " * "
   show Div = " / "


-- | apply the binary operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- | define data type for arithmetic expressions
data Expr = Val Int | App Op Expr Expr

-- | making Expr an instance of Show
instance Show Expr where
   show (Val x) =  show x
   show (App op l r) = fun l ++ show op ++ fun r
                       where
                          fun (Val n) = show n
                          fun ex      = "( " ++ show ex ++ " )"

-- | predicate for checking validity of operation of numbers
isValid :: Op -> Int -> Int -> Bool
isValid Add x y = x <= y
isValid Sub x y = x > y
isValid Mul x y = x /= 1 && y /= 1 && x <= y
isValid Div x y = y /= 1 && x `mod` y == 0

-- | reduction of expressions
vals :: Expr -> [Int]
vals (Val x)     = [x]
vals (App _ x y) = vals x ++ vals y

-- | evaluation of the actual expressions
eval :: Expr -> [Int]
eval (Val x)      = [x | x > 0]
eval (App op x y) = [apply op l r | l <- eval x, r <- eval y, isValid op l r]

-- | functions for subsequences and permutations of a list
subs :: [a] -> [[a]]
subs xs = [] : nonEmptySubs xs
     where
     nonEmptySubs [] = []
     nonEmptySubs (y : ys) = [y] : foldr (\a b -> a : (y : a) : b) [] (nonEmptySubs ys)

-- | permutations
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat $ map (ndinsert x) (perms xs)
      where
      -- non deterministic list insert
      ndinsert :: a -> [a] -> [[a]]
      ndinsert y []         = [[y]]
      ndinsert y t@(z : zs) = (y : t) : map (z:) (ndinsert y zs)

-- λ> subs "abc"
-- ["","a","b","ab","c","ac","bc","abc"]
-- λ> perms "abc"
-- ["abc","bac","bca","acb","cab","cba"]

-- | a combination function of both subs and perms
--   a function which returns all choices from a list given by all the
--   possible ways of selecting zero or more elements in any order
choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

-- λ> choices "abc"
-- ["","a","b","ab","ba","c","ac","ca","bc","cb","abc","bac","bca","acb","cab","cba"]


-- | split a list into pairs of lists
splitList :: [a] -> [([a], [a])]
splitList []       = []
splitList [_]      = []
splitList (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- splitList xs]

-- λ> mapM_ print $ splitList "abcde"
-- ("a","bcde")
-- ("ab","cde")
-- ("abc","de")
-- ("abcd","e")

-- | define a function which returns all the possible expressions whose
--   list of values is precisely a given list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [x] = [Val x]
exprs xs = [e | (ls, rs) <- splitList xs,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

-- | auxiliary functions for combining each pair of expressions
combine :: Expr -> Expr -> [Expr]
combine l r = [App op l r | op <- ops]
        where
        ops = [Add, Sub, Mul, Div]

-- | for solving
solver :: [Int] -> Int -> [Expr]
solver xs x = [ex | ys <- choices xs, ex <- exprs ys, eval ex == [x]]

test_solver :: IO ()
test_solver = mapM_ print $ solver [1,3,7,10,25,50] 765

-- | the solver function above works in a brute force manner and unnecessarily
--   checks a lot of negative combinations. To improve this we can combine the
--   generation and evaluation steps so that the evaluation step fails at a
--   very nascent step it self for negative cases; most importantly all of the
--   expressions which fail to evaluate are not used to generate any further
--   expressions that fail to evaluate.

type Result = (Expr, Int)

combineResult :: Result -> Result -> [Result]
combineResult (l, x) (r, y) = [(App op l r, apply op x y) | op <- [Add, Sub, Mul, Div],
                                                            isValid op x y]

-- | return all possible values comprising expressions whose list of values
--   is precisely a given list
results :: [Int] -> [Result]
results [] = []
results [x] = [(Val x, x) | x > 0]
results xs = [sol | (ls, rs) <- splitList xs,
                    lx       <- results ls,
                    rx       <- results rs,
                    sol      <- combineResult lx rx]

fastSolver :: [Int] -> Int -> [Expr]
fastSolver xs n = [ex | ys     <- choices xs,
                       (ex, m) <- results ys,
                       m == n]

-- | input arguments are the target number and list of integers
main :: IO ()
main = do
   (target : source) <- getArgs
   let a = read target :: Int
       b = read (head source) :: [Int]
   -- print a
   -- print b
   mapM_ print $ fastSolver b a

-- test run
-- λ> :main 14 [2,5,1,9]
-- 5 + 9
-- 1 + ( ( 2 * 9 ) - 5 )
-- ( 1 + ( 2 * 9 ) ) - 5
-- ( 2 * 9 ) - ( 5 - 1 )

-----------------------------------------------------------------------------------
-- | estimate the area under a function curve using integrals
--   with the Trapezoidal method where we divide the range of
--   interest (a, b) into equal sized parts
--   Take the function to be integrated as a parameter
--   width of the trapezoids as delta
--   range of interest is (a, b)

{-# LANGUAGE TypeFamilies #-}

{-|

Author     : Sampath Singamsetty
Maintainer :
File       : Haskercise10.hs
Description: General exrcises and problems in Haskell
-}

module Haskercise16
    (
    IsList(..),
    accept,
    number,
    identifier,
    operator,
    term,
    parse,
    tokenize,
    main
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

--
-- A NonEmpty list is a list which always has at least one element, but is
-- otherwise identical to the traditional list type in terms of complexity
-- and API.
data NonEmpty a = a :| [a]
    deriving (Eq, Ord, Show, Read)

instance IsList (NonEmpty a) where
    type Item (NonEmpty a) = a
    fromList               = fromList
    toList                 = toList
-----------------------------------------------------------------------------------
-- parser for a calculator
-- define data type for Operator
data Operator = Plus
              | Minus
              | Multiply
              | Divide
              deriving (Eq, Show)

-- define a data type for handling the individual tokens
data Token = TokenOp Operator
           | TokenAssign
           | TokenLeftParen
           | TokenRightParen
           | TokenId String
           | TokenNum Double
           | TokenEnd
           deriving (Show, Eq)

{-
a formal description of the grammar to be followed for parsing the expression

Expression <- Term [+ | -] Expression
            | Identifier '=' Expression
            | Term
Term       <- Factor [* | /] Term
            | Factor
Factor     <- Number
            | Identifier
            | [+ | -] Factor
            | '(' Expression ')'
-}

-- a function for parsing the individual operators
operator :: Char -> Operator
operator c
  | c == '+' = Plus
  | c == '-' = Minus
  | c == '*' = Multiply
  | c == '/' = Divide

-- tokenizing the String input
tokenize :: String -> [Token]
tokenize (c : cs)
  | c `elem` "+-*/" = TokenOp (operator c) : tokenize cs
  | c == '=' = TokenAssign : tokenize cs
  | c == '(' = TokenLeftParen : tokenize cs
  | c == ')' = TokenRightParen : tokenize cs
  | C.isDigit c = number c cs
  | C.isAlpha c = identifier c cs
  | C.isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize " ++ [c]

-- helper functions
number :: Char -> String -> [Token]
number c cs = let (digits, numlist) = span C.isDigit cs
              in TokenNum (read (c : digits)) : tokenize numlist

identifier :: Char -> String -> [Token]
identifier c cs = let (val, ids) = span C.isAlphaNum cs
                  in TokenId (c : val) : tokenize ids

-- parsing the expression, by considering it as a Tree
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignmentNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
          deriving (Show)

-- helper function for Tree
lookAhead :: [Token] -> Token
lookAhead ts = if null ts then TokenEnd else head ts

accept :: [Token] -> [Token]
accept ts = if null ts then error "Nothing to accept" else tail ts

expression :: [Token] -> (Tree, [Token])
expression tokens = let (termTree, restOfTokens) = term tokens
                    in
                     case lookAhead restOfTokens of
                       (TokenOp op)
                         | op `elem` [Plus, Minus] -> let (exprTree, remTokens) = expression (accept restOfTokens)
                                                     in (SumNode op termTree exprTree, remTokens)
                       TokenAssign ->
                         case termTree of
                           VarNode str -> let (exprTree, remTokens) = expression (accept restOfTokens)
                                         in (AssignmentNode str exprTree, remTokens)
                           _ -> error "Only variables can be assigned to"
                       _ -> (termTree, restOfTokens)

term :: [Token] -> (Tree, [Token])
term tokens = let (factorTree, restOfTokens) = factor tokens
              in
               case lookAhead restOfTokens of
                 (TokenOp op)
                   | op `elem` [Multiply, Divide] -> let (termTree, remTokens) = term (accept restOfTokens)
                                                    in (ProdNode op factorTree termTree, remTokens)
                 _ -> (factorTree, restOfTokens)

factor :: [Token] -> (Tree, [Token])
factor tokens = case lookAhead tokens of
                  (TokenNum x) -> (NumNode x, accept tokens)
                  (TokenId str) -> (VarNode str, accept tokens)
                  (TokenOp op)
                    | op `elem` [Plus, Minus] -> let (factorTree, restOfTokens) = factor (accept tokens)
                                                in (UnaryNode op factorTree, restOfTokens)
                  TokenLeftParen -> let (exprTree, restOfTokens) = expression (accept tokens)
                                   in
                                    if lookAhead restOfTokens /= TokenRightParen
                                    then error "Missing right parenthesis"
                                    else (exprTree, accept restOfTokens)
                  _ -> error $ "Parse error on token: " ++ show tokens

parse :: [Token] -> Tree
parse tokens = let (tree, restOfTokens) = expression tokens
               in
                if null restOfTokens
                then tree
                else error $ "Leftover tokens: " ++ show restOfTokens

evaluate :: Tree -> Double
evaluate (SumNode op left right) = let lft = evaluate left
                                       rgt = evaluate right
                                   in
                                    case op of
                                      Plus  -> lft + rgt
                                      Minus -> lft - rgt
evaluate (ProdNode op left right) = let lft = evaluate left
                                        rgt = evaluate right
                                    in
                                     case op of
                                       Multiply -> lft * rgt
                                       Divide   -> lft / rgt
evaluate (UnaryNode op tree) = let x = evaluate tree
                               in
                                case op of
                                  Plus  -> x
                                  Minus -> -x
evaluate (NumNode x) = x
evaluate (AssignmentNode _ tree) = evaluate tree
evaluate (VarNode _) = 0

main :: IO ()
main = (print . evaluate . parse . tokenize) "x = -12 / (3 + y)"

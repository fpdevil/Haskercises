{- | Binary Search Tree implementation in Haskell

Author     : Sampath Singamsetty
Maintainer :
File       : Tree.hs
Description: A simple binary tree datastructure in Haskell

-}

module Tree
(
singleton,
insert2tree,
list2tree,
tree2list,
invertTree,
treeMap,
treeElement,
height,
size,
preorder,
postorder,
inorder,
showtot,
showtot',
uptoTreeDepth,
nullTree
)
where

import qualified Data.List   as L
import           Debug.Trace (trace)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Read)

-- make tree an instance of Show for display purpose
-- printing taken shamelessly from the below link
-- http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/
instance (Show a) => Show (Tree a) where
    show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
        where
            -- if the tree is empty
            treeshow _ Empty = ""
            -- if tree has only one element or leaf. i.e., singleton tree
            treeshow pfx (Node x Empty Empty) = pshow pfx x
            -- if tree's right branch is empty
            treeshow pfx (Node x left Empty) = pshow pfx x ++
                                               "\n" ++
                                               sshow pfx "`--" "   " left
            -- if tree'ss left branch is empty
            treeshow pfx (Node x Empty right) = pshow pfx x ++
                                                "\n" ++
                                                sshow pfx "`--" "   " right
            -- when both left and right nodes are non empty
            treeshow pfx (Node x left right) = pshow pfx x ++
                                               "\n" ++
                                               sshow pfx "|--" "|  " left ++
                                               "\n" ++
                                               sshow pfx "`--" "   " right

            -- show tree using some prefixes for nicer tree display
            sshow pfx before next t = pfx ++ before ++ treeshow (pfx ++ next) t

            -- pshow for replacing "\n" with "\n" ++ pfx
            pshow pfx x = replace '\n' ("\n" ++ pfx) (show x)

            -- replace a character by a string
            replace ch newl = concatMap (change ch newl)
                where
                    change c n x
                        | x == c = n
                        | otherwise = [x] -- "x"


-- create a singleton tree which is a tree with only one element
singleton :: a -> Tree a
singleton x = Node x Empty Empty

-- inserting an element into the tree
insert2tree :: (Ord a) => a -> Tree a -> Tree a
insert2tree x Empty = singleton x
insert2tree x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (insert2tree x left) right
    | x > a = Node a left (insert2tree x right)

-- creation of tree.
-- this function coverts a list into an ordered binary tree
list2tree :: (Ord a) => [a] -> Tree a
list2tree []       = Empty
list2tree [x]      = singleton x
list2tree (x : xs) = Node x (list2tree (filter (< x) xs))
                            (list2tree (filter (> x) xs))

-- convert tree to list
tree2list :: (Ord a) => Tree a -> [a]
tree2list Empty                = []
tree2list (Node x Empty Empty) = [x]
tree2list (Node x left right)  = tree2list left ++ [x] ++ tree2list right

-- inverting a binary tree
invertTree :: (Ord a) => Tree a -> Tree a
invertTree Empty = Empty
invertTree t@(Node _ Empty Empty) = t
invertTree (Node x left right) = Node x (invertTree right) (invertTree left)

-- treeMap, applies a function to each node of tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty               = Empty
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)

-- check if an element is present in a Tree
treeElement :: (Ord a) => Tree a -> a -> Bool
treeElement Empty _ = False
treeElement (Node x left right) e
   | e == x = True
   | e < x = treeElement left e
   | e > x = treeElement right e


-- height of a binary tree
-- it is the longest path from root to leaf
height :: (Ord a) => Tree a -> Int
height Empty        = 0
height (Node x l r) = 1 + max (height l) (height r)

-- size or the number of nudes in a binary tree
size :: (Ord a) => Tree a -> Int
size Empty        = 0
size (Node x l r) = 1 + size l + size r

-- tree traversals
-- inorder tree traversal
-- In this we first visit the left subtree then the root
-- then we traverse the right subtree
inorder :: (Ord a) => Tree a -> [a]
inorder Empty        = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

-- preorder traversal
-- in this we first visit the root then the left subtree
-- and then we traverse the right subtree
preorder :: (Ord a) => Tree a -> [a]
preorder Empty        = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r

-- postorder traversal
-- in this we first vist the left subtree, then the right
-- subtree and finally visit the root
postorder :: (Ord a) => Tree a -> [a]
postorder Empty        = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]


-- tree of trees tot
-- Representing a char binary tree
showtot :: IO ()
showtot = print $ (list2tree . map list2tree) ["map", "hat", "cap"]

showtot' :: IO ()
showtot' = print $ (list2tree . map (list2tree . map list2tree)) [["TO", "EMACS"],
                                                                  ["YOU", "SAID"],
                                                                  ["YOU", "SAID"],
                                                                  ["WITH", "LOVE"]]


-- take depth of the tree
uptoTreeDepth :: (Eq a, Ord a) => Int -> Tree a -> Tree a
uptoTreeDepth _ Empty               = Empty
uptoTreeDepth 0 _                   = Empty
uptoTreeDepth n (Node x left right) = Node x l r
          where
            l = uptoTreeDepth (n - 1) left
            r = uptoTreeDepth (n - 1) right

-- an infinite binary tree
nullTree :: Tree Integer
nullTree = Node 0 nullTree nullTree

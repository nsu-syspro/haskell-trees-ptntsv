{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment

import Data.Foldable (Foldable (foldr))
import Task1 (Order (InOrder), Tree (..), torder)
import Prelude hiding (Ordering (..), compare, foldl, foldr)

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
compare :: (Ord a) => Cmp a
compare x y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
listToBST :: Cmp a -> [a] -> Tree a
listToBST cmp = foldr (tinsert cmp) Leaf

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
bstToList :: Tree a -> [a]
bstToList = torder InOrder Nothing

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
isBST :: Cmp a -> Tree a -> Bool
isBST _ Leaf = True
isBST _ (Branch _ Leaf Leaf) = True
isBST cmp (Branch val l@(Branch lval _ _) Leaf) = isBST cmp l && cmp lval val == LT
isBST cmp (Branch val Leaf r@(Branch rval _ _)) = isBST cmp r && cmp val rval == LT
isBST cmp (Branch val l@(Branch lval _ _) r@(Branch rval _ _)) = isBST cmp l && isBST cmp r && cmp lval val == LT && cmp val rval == LT

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp needle (Branch val l r)
  | cmp needle val == EQ = Just val
  | cmp needle val == LT = tlookup cmp needle l
  | otherwise = tlookup cmp needle r

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ x Leaf = Branch x Leaf Leaf
tinsert cmp x (Branch val l r)
  | cmp x val == EQ = Branch x l r
  | cmp x val == LT = Branch val (tinsert cmp x l) r
  | otherwise = Branch val l (tinsert cmp x r)

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp x (Branch val l r) =
  case cmp x val of
    EQ -> hangLeftmost l r
    LT -> Branch val (tdelete cmp x l) r
    GT -> Branch val l (tdelete cmp x r)
  where
    hangLeftmost :: Tree a -> Tree a -> Tree a
    hangLeftmost src Leaf = src
    hangLeftmost src (Branch val' l' r') = Branch val' (hangLeftmost src l') r'

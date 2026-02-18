{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment

import Data.Foldable (Foldable (foldr))
import Task1 (Order (InOrder), Tree (..), torder)
import Task2 (Cmp, Ordering (..), tdelete, tinsert, tlookup)
import Prelude hiding (Ordering (..), compare, foldl, foldr)

-- * Type definitions

-- | Tree-based map
type Map k v = Tree (k, v)

-- * Function definitions

--
-- >>> compareKeys  (1, 'e') (3, 'a')
-- True
compareKeys :: (Ord k) => Cmp (k, v)
compareKeys (k1, _) (k2, _)
  | k1 < k2 = LT
  | k1 == k2 = EQ
  | otherwise = GT

-- | Construction of 'Map' from association list
--
-- Usage example:
--
-- >>> listToMap [(2,'a'),(3,'c'),(1,'b')]
-- Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf)
-- >>> listToMap [] :: Map Int Char
-- Leaf
listToMap :: (Ord k) => [(k, v)] -> Map k v
listToMap = foldr (uncurry minsert) Leaf

-- | Conversion from 'Map' to association list sorted by key
--
-- Usage example:
--
-- >>> mapToList (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- [(1,'b'),(2,'a'),(3,'c')]
-- >>> mapToList Leaf
-- []
mapToList :: Map k v -> [(k, v)]
mapToList = torder InOrder Nothing

-- | Searches given 'Map' for a value associated with given key
--
-- Returns value associated with key wrapped into 'Just'
-- if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> mlookup 1 (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- Just 'b'
-- >>> mlookup 'a' Leaf
-- Nothing
mlookup :: (Ord k) => k -> Map k v -> Maybe v
mlookup x map' = case tlookup compareKeys (x, undefined) map' of
  Just (_, val) -> Just val
  Nothing -> Nothing

-- | Inserts given key and value into given 'Map'
--
-- If given key was already present in the 'Map'
-- then replaces its value with given value.
--
-- Usage example:
--
-- >>> minsert 0 'd' (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- Branch (2,'a') (Branch (1,'b') (Branch (0,'d') Leaf Leaf) Leaf) (Branch (3,'c') Leaf Leaf)
-- >>> minsert 1 'X' (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- Branch (2,'a') (Branch (1,'X') Leaf Leaf) (Branch (3,'c') Leaf Leaf)
-- >>> minsert 1 'X' Leaf
-- Branch (1,'X') Leaf Leaf
minsert :: (Ord k) => k -> v -> Map k v -> Map k v
minsert k v = tinsert compareKeys (k, v)

-- | Deletes given key from given 'Map'
--
-- Returns updated 'Map' if the key was present in it;
-- or unchanged 'Map' otherwise.
--
-- Usage example:
--
-- >>> mdelete 1 (Branch (2,'a') (Branch (1,'b') Leaf Leaf) (Branch (3,'c') Leaf Leaf))
-- Branch (2,'a') Leaf (Branch (3,'c') Leaf Leaf)
-- >>> mdelete 'a' Leaf
-- Leaf
mdelete :: (Ord k) => k -> Map k v -> Map k v
mdelete k' = tdelete compareKeys (k', undefined)

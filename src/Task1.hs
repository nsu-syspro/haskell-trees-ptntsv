{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show)

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving (Show)

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."
-- >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."
-- >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- NOW "...BA"
torder ::
  -- | Order of resulting traversal
  Order ->
  -- | Optional leaf value
  Maybe a ->
  -- | Tree to traverse
  Tree a ->
  -- | List of values in specified order
  [a]
torder _ Nothing Leaf = []
torder _ (Just lval) Leaf = [lval]
torder order lval (Branch val l r) = case order of
  PreOrder -> [val] ++ torder PreOrder lval l ++ torder PreOrder lval r
  InOrder -> torder InOrder lval l ++ [val] ++ torder InOrder lval r
  PostOrder -> torder PostOrder lval l ++ torder PostOrder lval r ++ [val]

-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|C..|A.B.."
-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|.C.|.A.B."
-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|..C|...BA"
forder ::
  -- | Order of tree traversal
  Order ->
  -- | Optional separator between resulting tree orders
  Maybe a ->
  -- | Optional leaf value
  Maybe a ->
  -- | List of trees to traverse
  Forest a ->
  -- | List of values in specified tree order
  [a]
forder ord msep lval forest = intercalate (maybeToList msep) (map (torder ord lval) forest)

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x : xs) = [x, sep] ++ intersperse sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

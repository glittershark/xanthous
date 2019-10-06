{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Xanthous.Util
  ( EqEqProp(..)
  , EqProp(..)
  , foldlMapM
  , foldlMapM'
  , between

  , appendVia

    -- * Foldable
    -- ** Uniqueness
    -- *** Predicates on uniqueness
  , isUniqueOf
  , isUnique
    -- *** Removing all duplicate elements in n * log n time
  , uniqueOf
  , unique
    -- *** Removing sequentially duplicate elements in linear time
  , uniqOf
  , uniq
    -- ** Bag sequence algorithms
  , takeWhileInclusive
  , smallestNotIn
  ) where

import Xanthous.Prelude hiding (foldr)

import Test.QuickCheck.Checkers
import Data.Foldable (foldr)
import Data.Monoid

newtype EqEqProp a = EqEqProp a
  deriving newtype Eq

instance Eq a => EqProp (EqEqProp a) where
  (=-=) = eq

foldlMapM :: forall g b a m. (Foldable g, Monoid b, Applicative m) => (a -> m b) -> g a -> m b
foldlMapM f = foldr f' (pure mempty)
  where
    f' :: a -> m b -> m b
    f' x = liftA2 mappend (f x)

-- Strict in the monoidal accumulator. For monads strict
-- in the left argument of bind, this will run in constant
-- space.
foldlMapM' :: forall g b a m. (Foldable g, Monoid b, Monad m) => (a -> m b) -> g a -> m b
foldlMapM' f xs = foldr f' pure xs mempty
  where
  f' :: a -> (b -> m b) -> b -> m b
  f' x k bl = do
    br <- f x
    let !b = mappend bl br
    k b

between
  :: Ord a
  => a -- ^ lower bound
  -> a -- ^ upper bound
  -> a -- ^ scrutinee
  -> Bool
between lower upper x = x >= lower && x <= upper

-- |
-- >>> appendVia Sum 1 2
-- 3
appendVia :: (Rewrapping s t, Semigroup s) => (Unwrapped s -> s) -> Unwrapped s -> Unwrapped s -> Unwrapped s
appendVia wrap x y = op wrap $ wrap x <> wrap y

--------------------------------------------------------------------------------

-- | Returns True if the targets of the given 'Fold' are unique per the 'Ord' instance for @a@
--
-- >>> isUniqueOf (folded . _1) ([(1, 2), (2, 2), (3, 2)] :: [(Int, Int)])
-- True
--
-- >>> isUniqueOf (folded . _2) ([(1, 2), (2, 2), (3, 2)] :: [(Int, Int)])
-- False
--
-- @
-- 'isUniqueOf' :: Ord a => 'Getter' s a     -> s -> 'Bool'
-- 'isUniqueOf' :: Ord a => 'Fold' s a       -> s -> 'Bool'
-- 'isUniqueOf' :: Ord a => 'Lens'' s a      -> s -> 'Bool'
-- 'isUniqueOf' :: Ord a => 'Iso'' s a       -> s -> 'Bool'
-- 'isUniqueOf' :: Ord a => 'Traversal'' s a -> s -> 'Bool'
-- 'isUniqueOf' :: Ord a => 'Prism'' s a     -> s -> 'Bool'
-- @
isUniqueOf :: Ord a => Getting (Endo (Set a, Bool)) s a -> s -> Bool
isUniqueOf aFold = orOf _2 . foldrOf aFold rejectUnique (mempty, True)
 where
  rejectUnique x (seen, acc)
    | seen ^. contains x = (seen, False)
    | otherwise          = (seen & contains x .~ True, acc)

-- | Returns true if the given 'Foldable' container contains only unique
-- elements, as determined by the 'Ord' instance for @a@
--
-- >>> isUnique ([3, 1, 2] :: [Int])
-- True
--
-- >>> isUnique ([1, 1, 2, 2, 3, 1] :: [Int])
-- False
isUnique :: (Foldable f, Ord a) => f a -> Bool
isUnique = isUniqueOf folded


-- | O(n * log n). Returns a monoidal, 'Cons'able container (a list, a Set,
-- etc.) consisting of the unique (per the 'Ord' instance for @a@) targets of
-- the given 'Fold'
--
-- >>> uniqueOf (folded . _2) ([(1, 2), (2, 2), (3, 2), (4, 3)] :: [(Int, Int)]) :: [Int]
-- [2,3]
--
-- @
-- 'uniqueOf' :: Ord a => 'Getter' s a     -> s -> [a]
-- 'uniqueOf' :: Ord a => 'Fold' s a       -> s -> [a]
-- 'uniqueOf' :: Ord a => 'Lens'' s a      -> s -> [a]
-- 'uniqueOf' :: Ord a => 'Iso'' s a       -> s -> [a]
-- 'uniqueOf' :: Ord a => 'Traversal'' s a -> s -> [a]
-- 'uniqueOf' :: Ord a => 'Prism'' s a     -> s -> [a]
-- @
uniqueOf
  :: (Monoid c, Ord w, Cons c c w w) => Getting (Endo (Set w, c)) a w -> a -> c
uniqueOf aFold = snd . foldrOf aFold rejectUnique (mempty, mempty)
 where
  rejectUnique x (seen, acc)
    | seen ^. contains x = (seen, acc)
    | otherwise          = (seen & contains x .~ True, cons x acc)

-- | Returns a monoidal, 'Cons'able container (a list, a Set, etc.) consisting
-- of the unique (per the 'Ord' instance for @a@) contents of the given
-- 'Foldable' container
--
-- >>> unique [1, 1, 2, 2, 3, 1] :: [Int]
-- [2,3,1]

-- >>> unique [1, 1, 2, 2, 3, 1] :: Set Int
-- fromList [3,2,1]
unique :: (Foldable f, Cons c c a a, Ord a, Monoid c) => f a -> c
unique = uniqueOf folded

--------------------------------------------------------------------------------

-- | O(n). Returns a monoidal, 'Cons'able container (a list, a Vector, etc.)
-- consisting of the targets of the given 'Fold' with sequential duplicate
-- elements removed
--
-- This function (sorry for the confusing name) differs from 'uniqueOf' in that
-- it only compares /sequentially/ duplicate elements (and thus operates in
-- linear time).
-- cf 'Data.Vector.uniq' and POSIX @uniq@ for the name
--
-- >>> uniqOf (folded . _2) ([(1, 2), (2, 2), (3, 1), (4, 2)] :: [(Int, Int)]) :: [Int]
-- [2,1,2]
--
-- @
-- 'uniqOf' :: Eq a => 'Getter' s a     -> s -> [a]
-- 'uniqOf' :: Eq a => 'Fold' s a       -> s -> [a]
-- 'uniqOf' :: Eq a => 'Lens'' s a      -> s -> [a]
-- 'uniqOf' :: Eq a => 'Iso'' s a       -> s -> [a]
-- 'uniqOf' :: Eq a => 'Traversal'' s a -> s -> [a]
-- 'uniqOf' :: Eq a => 'Prism'' s a     -> s -> [a]
-- @
uniqOf :: (Monoid c, Cons c c w w, Eq w) => Getting (Endo (Maybe w, c)) a w -> a -> c
uniqOf aFold = snd . foldrOf aFold rejectSeen (Nothing, mempty)
  where
    rejectSeen x (Nothing, acc) = (Just x, x <| acc)
    rejectSeen x tup@(Just a, acc)
      | x == a     = tup
      | otherwise = (Just x, x <| acc)

-- | O(n). Returns a monoidal, 'Cons'able container (a list, a Vector, etc.)
-- consisting of the targets of the given 'Foldable' container with sequential
-- duplicate elements removed
--
-- This function (sorry for the confusing name) differs from 'unique' in that
-- it only compares /sequentially/ unique elements (and thus operates in linear
-- time).
-- cf 'Data.Vector.uniq' and POSIX @uniq@ for the name
--
-- >>> uniq [1, 1, 1, 2, 2, 2, 3, 3, 1] :: [Int]
-- [1,2,3,1]
--
-- >>> uniq [1, 1, 1, 2, 2, 2, 3, 3, 1] :: Vector Int
-- [1,2,3,1]
--
uniq :: (Foldable f, Eq a, Cons c c a a, Monoid c) => f a -> c
uniq = uniqOf folded

-- | Like 'takeWhile', but inclusive
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

-- | Returns the smallest value not in a list
smallestNotIn :: (Ord a, Bounded a, Enum a) => [a] -> a
smallestNotIn xs = case uniq $ sort xs of
  [] -> minBound
  xs'@(x : _)
    | x > minBound -> minBound
    | otherwise
    -> snd . headEx . filter (uncurry (/=)) $ zip (xs' ++ [minBound]) [minBound..]

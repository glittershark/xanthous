{-# LANGUAGE BangPatterns #-}

module Xanthous.Util
  ( EqEqProp(..)
  , EqProp(..)
  , foldlMapM
  , foldlMapM'
  , between
  ) where

import Xanthous.Prelude hiding (foldr)

import Test.QuickCheck.Checkers
import Data.Foldable (foldr)

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

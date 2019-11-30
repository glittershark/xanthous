--------------------------------------------------------------------------------
module Xanthous.Prelude
  ( module ClassyPrelude
  , Type
  , Constraint
  , module GHC.TypeLits
  , module Control.Lens
  , module Data.Void
  , module Control.Comonad


    -- * Classy-Prelude addons
  , ninsertSet
  , ndeleteSet
  , toVector
  ) where
--------------------------------------------------------------------------------
import ClassyPrelude hiding
  (return, (<|), unsnoc, uncons, cons, snoc, index, (<.>), Index, say)
import Data.Kind
import GHC.TypeLits hiding (Text)
import Control.Lens
import Data.Void
import Control.Comonad
--------------------------------------------------------------------------------

ninsertSet
  :: (IsSet set, MonoPointed set)
  => Element set -> NonNull set -> NonNull set
ninsertSet x xs = impureNonNull $ opoint x `union` toNullable xs

ndeleteSet :: IsSet b => Element b -> NonNull b -> b
ndeleteSet x = deleteSet x . toNullable

toVector :: (MonoFoldable (f a), Element (f a) ~ a) => f a -> Vector a
toVector = fromList . toList

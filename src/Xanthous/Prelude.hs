--------------------------------------------------------------------------------
module Xanthous.Prelude
  ( module ClassyPrelude
  , Type
  , Constraint
  , module GHC.TypeLits
  , module Control.Lens
  , module Data.Void
  , module Control.Comonad
  , module Witherable
  , fail

  , (&!)

    -- * Classy-Prelude addons
  , ninsertSet
  , ndeleteSet
  , toVector
  ) where
--------------------------------------------------------------------------------
import ClassyPrelude hiding
  ( return, (<|), unsnoc, uncons, cons, snoc, index, (<.>), Index, say
  , catMaybes, filter, mapMaybe, hashNub, ordNub
  , Memoized, runMemoized
  )
import Data.Kind
import GHC.TypeLits hiding (Text)
import Control.Lens hiding (levels, Level)
import Data.Void
import Control.Comonad
import Witherable
import Control.Monad.Fail (fail)
--------------------------------------------------------------------------------

ninsertSet
  :: (IsSet set, MonoPointed set)
  => Element set -> NonNull set -> NonNull set
ninsertSet x xs = impureNonNull $ opoint x `union` toNullable xs

ndeleteSet :: IsSet b => Element b -> NonNull b -> b
ndeleteSet x = deleteSet x . toNullable

toVector :: (MonoFoldable (f a), Element (f a) ~ a) => f a -> Vector a
toVector = fromList . toList

infixl 1 &!
(&!) :: a -> (a -> b) -> b
(&!) = flip ($!)

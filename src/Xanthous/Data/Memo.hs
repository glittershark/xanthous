--------------------------------------------------------------------------------
-- | Memoized values
--------------------------------------------------------------------------------
module Xanthous.Data.Memo
  ( Memoized(UnMemoized)
  , memoizeWith
  , getMemoized
  , runMemoized
  , fillWith
  , fillWithM
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Test.QuickCheck (Arbitrary (arbitrary), oneof, CoArbitrary, Function)
import Test.QuickCheck.Checkers (EqProp)
import Xanthous.Util (EqEqProp(EqEqProp))
import Control.Monad.State.Class (MonadState)
--------------------------------------------------------------------------------

-- | A memoized value, keyed by a key
--
-- If key is different than what is stored here, then val is invalid
data Memoized key val = Memoized key val | UnMemoized
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, FromJSON, ToJSON, NFData, CoArbitrary, Function)
  deriving EqProp via EqEqProp (Memoized key val)

instance (Arbitrary k, Arbitrary v) => Arbitrary (Memoized k v) where
  arbitrary = oneof [ pure UnMemoized
                    , Memoized <$> arbitrary <*> arbitrary
                    ]

-- | Construct a memoized value with the given key
memoizeWith :: forall key val. key -> val -> Memoized key val
memoizeWith = Memoized
{-# INLINE memoizeWith #-}

-- | Retrieve a memoized value providing the key. If the value is unmemoized or
-- the keys do not match, returns Nothing.
--
-- >>> getMemoized 1 (memoizeWith @Int @Int 1 2)
-- Just 2
--
-- >>> getMemoized 2 (memoizeWith @Int @Int 1 2)
-- Nothing
--
-- >>> getMemoized 1 (UnMemoized :: Memoized Int Int)
-- Nothing
getMemoized :: Eq key => key -> Memoized key val -> Maybe val
getMemoized key (Memoized key' v)
  | key == key' = Just v
  | otherwise = Nothing
getMemoized _ UnMemoized = Nothing
{-# INLINE getMemoized #-}

-- | Get a memoized value using an applicative action to obtain the key
runMemoized
  :: (Eq key, Applicative m)
  => Memoized key val
  -> m key
  -> m (Maybe val)
runMemoized m mk = getMemoized <$> mk <*> pure m

-- | In a monadic state containing a 'MemoState', look up the current memoized
-- target of some lens keyed by k, filling it with v if not present and
-- returning either the new or old value
fillWith
  :: forall m s k v.
    (MonadState s m, Eq k)
  => Lens' s (Memoized k v)
  -> k
  -> v
  -> m v
fillWith l k v' = do
  uses l (getMemoized k) >>= \case
    Just v -> pure v
    Nothing -> do
      l .= memoizeWith k v'
      pure v'

-- | In a monadic state, look up the current memoized target of some lens keyed
-- by k, filling it with the result of some monadic action v if not present and
-- returning either the new or old value
fillWithM
  :: forall m s k v.
    (MonadState s m, Eq k)
  => Lens' s (Memoized k v)
  -> k
  -> m v
  -> m v
fillWithM l k mv = do
  uses l (getMemoized k) >>= \case
    Just v -> pure v
    Nothing -> do
      v' <- mv
      l .= memoizeWith k v'
      pure v'

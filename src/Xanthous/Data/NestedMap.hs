{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE PolyKinds             #-}
--------------------------------------------------------------------------------
module Xanthous.Data.NestedMap
  ( NestedMapVal(..)
  , NestedMap(..)
  , lookup
  , lookupVal
  , insert

    -- *
  , (:->)
  , BifunctorFunctor'(..)
  , BifunctorMonad'(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (lookup, foldMap)
import qualified Xanthous.Prelude as P
--------------------------------------------------------------------------------
import           Test.QuickCheck
import           Data.Aeson
import           Data.Function (fix)
import           Data.Foldable (Foldable(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
--------------------------------------------------------------------------------

-- | Natural transformations on bifunctors
type (:->) p q = forall a b. p a b -> q a b
infixr 0 :->

class (forall b. Bifunctor b => Bifunctor (t b)) => BifunctorFunctor' t where
  bifmap' :: (Bifunctor p, Bifunctor q) => (p :-> q) -> t p :-> t q

class BifunctorFunctor' t => BifunctorMonad' t where
  bireturn' :: (Bifunctor p) => p :-> t p

  bibind' :: (Bifunctor p, Bifunctor q) => (p :-> t q) -> t p :-> t q
  bibind' f = bijoin' . bifmap' f

  bijoin' :: (Bifunctor p) => t (t p) :-> t p
  bijoin' = bibind' id

  {-# MINIMAL bireturn', (bibind' | bijoin') #-}

--------------------------------------------------------------------------------

data NestedMapVal m k v = Val v | Nested (NestedMap m k v)

deriving stock instance
  ( forall k' v'. (Show k', Show v') => Show (m k' v')
  , Show k
  , Show v
  ) => Show (NestedMapVal m k v)

deriving stock instance
  ( forall k' v'. (Eq k', Eq v') => Eq (m k' v')
  , Eq k
  , Eq v
  ) => Eq (NestedMapVal m k v)

instance
  forall m k v.
  ( Arbitrary (m k v)
  , Arbitrary (m k (NestedMapVal m k v))
  , Arbitrary k
  , Arbitrary v
  , IsMap (m k (NestedMapVal m k v))
  , MapValue (m k (NestedMapVal m k v)) ~ (NestedMapVal m k v)
  , ContainerKey (m k (NestedMapVal m k v)) ~ k
  ) => Arbitrary (NestedMapVal m k v) where
  arbitrary = sized . fix $ \gen n ->
    let nst = fmap (NestedMap . mapFromList)
            . listOf
            $ (,) <$> arbitrary @k <*> gen (n `div` 2)
    in if n == 0
       then Val <$> arbitrary
       else oneof [ Val <$> arbitrary
                  , Nested <$> nst]
  shrink (Val v) = Val <$> shrink v
  shrink (Nested mkv) = Nested <$> shrink mkv

instance Functor (m k) => Functor (NestedMapVal m k) where
  fmap f (Val v) = Val $ f v
  fmap f (Nested m) = Nested $ fmap f m

instance Bifunctor m => Bifunctor (NestedMapVal m) where
  bimap _ g (Val v) = Val $ g v
  bimap f g (Nested m) = Nested $ bimap f g m

instance BifunctorFunctor' NestedMapVal where
  bifmap' _ (Val v) = Val v
  bifmap' f (Nested m) = Nested $ bifmap' f m

instance (ToJSONKey k, ToJSON v, ToJSON (m k (NestedMapVal m k v)))
       => ToJSON (NestedMapVal m k v) where
  toJSON (Val v) = toJSON v
  toJSON (Nested m) = toJSON m

instance Foldable (m k) => Foldable (NestedMapVal m k) where
  foldMap f (Val v) = f v
  foldMap f (Nested m) = foldMap f m

-- _NestedMapVal
--   :: forall m k v m' k' v'.
--     ( IsMap (m k v), IsMap (m' k' v')
--     , IsMap (m [k] v), IsMap (m' [k'] v')
--     , ContainerKey (m k v) ~ k, ContainerKey (m' k' v') ~ k'
--     , ContainerKey (m [k] v) ~ [k], ContainerKey (m' [k'] v') ~ [k']
--     , MapValue (m k v) ~ v, MapValue (m' k' v') ~ v'
--     , MapValue (m [k] v) ~ v, MapValue (m' [k'] v') ~ v'
--     )
--   => Iso (NestedMapVal m k v)
--         (NestedMapVal m' k' v')
--         (m [k] v)
--         (m' [k'] v')
-- _NestedMapVal = iso hither yon
--   where
--     hither :: NestedMapVal m k v -> m [k] v
--     hither (Val v) = singletonMap [] v
--     hither (Nested m) = bimap _ _ $ m ^. _NestedMap
--     yon = _

--------------------------------------------------------------------------------

newtype NestedMap m k v = NestedMap (m k (NestedMapVal m k v))

deriving stock instance
  ( forall k' v'. (Eq k', Eq v') => Eq (m k' v')
  , Eq k
  , Eq v
  ) => Eq (NestedMap m k v)

deriving stock instance
  ( forall k' v'. (Show k', Show v') => Show (m k' v')
  , Show k
  , Show v
  ) => Show (NestedMap m k v)

instance Arbitrary (m k (NestedMapVal m k v))
       => Arbitrary (NestedMap m k v) where
  arbitrary = NestedMap <$> arbitrary
  shrink (NestedMap m) = NestedMap <$> shrink m

instance Functor (m k) => Functor (NestedMap m k) where
  fmap f (NestedMap m) = NestedMap $ fmap (fmap f) m

instance Bifunctor m => Bifunctor (NestedMap m) where
  bimap f g (NestedMap m) = NestedMap $ bimap f (bimap f g) m

instance BifunctorFunctor' NestedMap where
  bifmap' f (NestedMap m) = NestedMap . f $ bimap id (bifmap' f) m

instance (ToJSONKey k, ToJSON v, ToJSON (m k (NestedMapVal m k v)))
       => ToJSON (NestedMap m k v) where
  toJSON (NestedMap m) = toJSON m

instance Foldable (m k) => Foldable (NestedMap m k) where
  foldMap f (NestedMap m) = foldMap (foldMap f) m

--------------------------------------------------------------------------------

lookup
  :: ( IsMap (m k (NestedMapVal m k v))
    , MapValue (m k (NestedMapVal m k v)) ~ (NestedMapVal m k v)
    , ContainerKey (m k (NestedMapVal m k v)) ~ k
    )
  => NonEmpty k
  -> NestedMap m k v
  -> Maybe (NestedMapVal m k v)
lookup (p :| []) (NestedMap vs) = P.lookup p vs
lookup (p :| (p₁ : ps)) (NestedMap vs) = P.lookup p vs >>= \case
  (Val _) -> Nothing
  (Nested vs') -> lookup (p₁ :| ps) vs'

lookupVal
  :: ( IsMap (m k (NestedMapVal m k v))
    , MapValue (m k (NestedMapVal m k v)) ~ (NestedMapVal m k v)
    , ContainerKey (m k (NestedMapVal m k v)) ~ k
    )
  => NonEmpty k
  -> NestedMap m k v
  -> Maybe v
lookupVal ks m
  | Just (Val v) <- lookup ks m = Just v
  | otherwise                  = Nothing

insert
  :: ( IsMap (m k (NestedMapVal m k v))
    , MapValue (m k (NestedMapVal m k v)) ~ (NestedMapVal m k v)
    , ContainerKey (m k (NestedMapVal m k v)) ~ k
    )
  => NonEmpty k
  -> v
  -> NestedMap m k v
  -> NestedMap m k v
insert (k :| []) v (NestedMap m) = NestedMap $ P.insertMap k (Val v) m
insert (k₁ :| (k₂ : ks)) v (NestedMap m) = NestedMap $ alterMap upd k₁ m
  where
    upd (Just (Nested nm)) = Just . Nested $ insert (k₂ :| ks) v nm
    upd _ = Just $
      let (kΩ :| ks') = NE.reverse (k₂ :| ks)
      in P.foldl'
         (\m' k -> Nested . NestedMap . singletonMap k $ m')
         (Nested . NestedMap . singletonMap kΩ $ Val v)
         ks'

-- _NestedMap
--   :: ( IsMap (m k v), IsMap (m' k' v')
--     , IsMap (m (NonEmpty k) v), IsMap (m' (NonEmpty k') v')
--     , ContainerKey (m k v) ~ k, ContainerKey (m' k' v') ~ k'
--     , ContainerKey (m (NonEmpty k) v) ~ (NonEmpty k)
--     , ContainerKey (m' (NonEmpty k') v') ~ (NonEmpty k')
--     , MapValue (m k v) ~ v, MapValue (m' k' v') ~ v'
--     , MapValue (m (NonEmpty k) v) ~ v, MapValue (m' (NonEmpty k') v') ~ v'
--     )
--   => Iso (NestedMap m k v)
--         (NestedMap m' k' v')
--         (m (NonEmpty k) v)
--         (m' (NonEmpty k') v')
-- _NestedMap = iso undefined yon
--   where
--     hither (NestedMap m) = undefined . mapToList $ m
--     yon mkv = undefined

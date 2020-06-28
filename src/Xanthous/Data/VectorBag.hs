{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Data.VectorBag
  (VectorBag(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Data.Aeson
import qualified Data.Vector as V
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Vector ()
--------------------------------------------------------------------------------

-- | Acts exactly like a Vector, except ignores order when testing for equality
newtype VectorBag a = VectorBag (Vector a)
  deriving stock
    ( Traversable
    , Generic
    )
  deriving newtype
    ( Show
    , Read
    , Foldable
    , FromJSON
    , FromJSON1
    , ToJSON
    , Reversing
    , Applicative
    , Functor
    , Monad
    , Monoid
    , Semigroup
    , Arbitrary
    , CoArbitrary
    , Filterable
    )
makeWrapped ''VectorBag

instance Function a => Function (VectorBag a) where
  function = functionMap (\(VectorBag v) -> v) VectorBag

type instance Element (VectorBag a) = a
deriving via (Vector a) instance MonoFoldable (VectorBag a)
deriving via (Vector a) instance GrowingAppend (VectorBag a)
deriving via (Vector a) instance SemiSequence (VectorBag a)
deriving via (Vector a) instance MonoPointed (VectorBag a)
deriving via (Vector a) instance MonoFunctor (VectorBag a)

instance Cons (VectorBag a) (VectorBag b) a b where
  _Cons = prism (\(x, VectorBag xs) -> VectorBag $ x <| xs) $ \(VectorBag v) ->
    if V.null v
    then Left (VectorBag mempty)
    else Right (V.unsafeHead v, VectorBag $ V.unsafeTail v)

instance AsEmpty (VectorBag a) where
  _Empty = prism' (const $ VectorBag Empty) $ \case
    (VectorBag Empty) -> Just ()
    _ -> Nothing

instance Witherable VectorBag where
  wither f (VectorBag v) = VectorBag <$> wither f v
  witherM f (VectorBag v) = VectorBag <$> witherM f v
  filterA p (VectorBag v) = VectorBag <$> filterA p v

{-
    TODO:
    , Ixed
    , FoldableWithIndex
    , FunctorWithIndex
    , TraversableWithIndex
    , Snoc
    , Each
-}

instance Ord a => Eq (VectorBag a) where
  (==) = (==) `on` (view _Wrapped . sort)

instance Ord a => Ord (VectorBag a) where
  compare = compare  `on` (view _Wrapped . sort)

instance MonoTraversable (VectorBag a) where
  otraverse f (VectorBag v) = VectorBag <$> otraverse f v

instance IsSequence (VectorBag a) where
  fromList = VectorBag . fromList
  break prd (VectorBag v) = bimap VectorBag VectorBag $ break prd v
  span prd (VectorBag v) = bimap VectorBag VectorBag $ span prd v
  dropWhile prd (VectorBag v) = VectorBag $ dropWhile prd v
  takeWhile prd (VectorBag v) = VectorBag $ takeWhile prd v
  splitAt idx (VectorBag v) = bimap VectorBag VectorBag $ splitAt idx v
  unsafeSplitAt idx (VectorBag v) =
    bimap VectorBag VectorBag $ unsafeSplitAt idx v
  take n (VectorBag v) = VectorBag $ take n v
  unsafeTake n (VectorBag v) = VectorBag $ unsafeTake n v
  drop n (VectorBag v) = VectorBag $ drop n v
  unsafeDrop n (VectorBag v) = VectorBag $ unsafeDrop n v
  partition p (VectorBag v) = bimap VectorBag VectorBag $ partition p v

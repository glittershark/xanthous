{-# LANGUAGE GADTs #-}
module Xanthous.Entities.SomeEntity
  ( SomeEntity(..)
  , downcastEntity
  ) where

import Xanthous.Prelude
import Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck.Gen as Gen

import Xanthous.Entities (Draw(..), Entity)
import Data.Typeable
import Xanthous.Entities.Character

data SomeEntity where
  SomeEntity :: forall a. (Entity a, Typeable a) => a -> SomeEntity

instance Show SomeEntity where
  show (SomeEntity x) = "SomeEntity (" <> show x <> ")"

instance Eq SomeEntity where
  (SomeEntity (a :: ea)) == (SomeEntity (b :: eb)) = case eqT @ea @eb of
    Just Refl -> a == b
    _ -> False

instance Arbitrary SomeEntity where
  arbitrary = Gen.oneof
    [pure $ SomeEntity Character]

instance Draw SomeEntity where
  draw (SomeEntity ent) = draw ent

downcastEntity :: (Entity a, Typeable a) => SomeEntity -> Maybe a
downcastEntity (SomeEntity e) = cast e

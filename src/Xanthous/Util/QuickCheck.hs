{-# LANGUAGE UndecidableInstances #-}
module Xanthous.Util.QuickCheck
  ( functionShow
  , FunctionShow(..)
  , functionJSON
  , FunctionJSON(..)
  , genericArbitrary
  , GenericArbitrary(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Arbitrary.Generic
import Data.Aeson
import GHC.Generics (Rep)
--------------------------------------------------------------------------------

newtype FunctionShow a = FunctionShow a
  deriving newtype (Show, Read)

instance (Show a, Read a) => Function (FunctionShow a) where
  function = functionShow

functionJSON :: (ToJSON a, FromJSON a) => (a -> c) -> a :-> c
functionJSON = functionMap encode (headEx . decode)

newtype FunctionJSON a = FunctionJSON a
  deriving newtype (ToJSON, FromJSON)

instance (ToJSON a, FromJSON a) => Function (FunctionJSON a) where
  function = functionJSON

--------------------------------------------------------------------------------

newtype GenericArbitrary a = GenericArbitrary a
  deriving newtype Generic

instance (Generic a, GArbitrary rep, Rep a ~ rep)
  => Arbitrary (GenericArbitrary a) where
  arbitrary = genericArbitrary

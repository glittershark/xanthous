module Xanthous.Util.QuickCheck
  ( FunctionShow(..)
  , functionJSON
  , FunctionJSON(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances.ByteString ()
import Data.Aeson
import Data.Coerce
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

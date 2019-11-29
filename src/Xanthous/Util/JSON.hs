--------------------------------------------------------------------------------
module Xanthous.Util.JSON
  ( ReadShowJSON(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Data.Aeson
--------------------------------------------------------------------------------

newtype ReadShowJSON a = ReadShowJSON a
  deriving newtype (Read, Show)

instance Show a => ToJSON (ReadShowJSON a) where
  toJSON = toJSON . show

instance Read a => FromJSON (ReadShowJSON a) where
  parseJSON = withText "readable"
    $ maybe (fail "Could not read") pure . readMay

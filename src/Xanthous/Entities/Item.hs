{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Item
  ( Item(..)
  , itemType
  , newWithType
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Test.QuickCheck
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Aeson.Generic.DerivingVia
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes hiding (Item, description)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Entities (Draw(..), Entity(..), DrawRawChar(..))
--------------------------------------------------------------------------------

data Item = Item
  { _itemType :: ItemType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (CoArbitrary, Function)
  deriving Draw via DrawRawChar "_itemType" Item
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Item
makeLenses ''Item

instance Arbitrary Item where
  arbitrary = Item <$> arbitrary

instance Entity Item where
  blocksVision _ = False
  description = view $ itemType . Raw.description

newWithType :: ItemType -> Item
newWithType = Item

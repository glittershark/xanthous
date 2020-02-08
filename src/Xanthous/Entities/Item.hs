{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Item
  ( Item(..)
  , itemType
  , newWithType
  , isEdible
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Test.QuickCheck
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Aeson.Generic.DerivingVia
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes hiding (Item, description, isEdible)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Game.State
--------------------------------------------------------------------------------

data Item = Item
  { _itemType :: ItemType
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Draw via DrawRawChar "_itemType" Item
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Item
makeLenses ''Item

{-# ANN Item ("HLint: ignore Use newtype instead of data" :: String )#-}

-- deriving via (Brainless Item) instance Brain Item
instance Brain Item where step = brainVia Brainless

instance Arbitrary Item where
  arbitrary = Item <$> arbitrary

instance Entity Item where
  description = view $ itemType . Raw.description
  entityChar = view $ itemType . Raw.char
  entityCollision = const Nothing

newWithType :: ItemType -> Item
newWithType = Item

isEdible :: Item -> Bool
isEdible = Raw.isEdible . view itemType

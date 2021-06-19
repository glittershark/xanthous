{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Item
  ( Item(..)
  , itemType
  , density
  , volume
  , newWithType
  , isEdible
  , weight
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Test.QuickCheck (Arbitrary, CoArbitrary, Function)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Aeson.Generic.DerivingVia
import           Control.Monad.Random (MonadRandom)
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes (ItemType)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Game.State
import           Xanthous.Data (Grams, Per, Cubic, Meters, (|*|))
import           Xanthous.Util.QuickCheck (GenericArbitrary(GenericArbitrary))
import           Xanthous.Random (choose, FiniteInterval(..))
--------------------------------------------------------------------------------

data Item = Item
  { _itemType :: ItemType
  , _density  :: Grams `Per` Cubic Meters
  , _volume   :: Cubic Meters
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Draw via DrawRawChar "_itemType" Item
  deriving Arbitrary via GenericArbitrary Item
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Item
makeLenses ''Item

-- deriving via (Brainless Item) instance Brain Item
instance Brain Item where step = brainVia Brainless

instance Entity Item where
  description = view $ itemType . Raw.description
  entityChar = view $ itemType . Raw.char
  entityCollision = const Nothing

newWithType :: MonadRandom m => ItemType -> m Item
newWithType _itemType = do
  _density <- choose . FiniteInterval $ _itemType ^. Raw.density
  _volume  <- choose . FiniteInterval $ _itemType ^. Raw.volume
  pure Item {..}

isEdible :: Item -> Bool
isEdible = Raw.isEdible . view itemType

-- | The weight of this item, calculated by multiplying its volume by the
-- density of its material
weight :: Item -> Grams
weight item = (item ^. density) |*| (item ^. volume)

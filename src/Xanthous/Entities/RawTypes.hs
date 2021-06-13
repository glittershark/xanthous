{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.RawTypes
  (
    EntityRaw(..)
  , _Creature
  , _Item

    -- * Creatures
  , CreatureType(..)
  , hostile

    -- * Items
  , ItemType(..)
    -- ** Item sub-types
    -- *** Edible
  , EdibleItem(..)
  , isEdible
    -- *** Wieldable
  , WieldableItem(..)
  , isWieldable

    -- * Lens classes
  , HasAttackMessage(..)
  , HasChar(..)
  , HasDamage(..)
  , HasDescription(..)
  , HasEatMessage(..)
  , HasEdible(..)
  , HasFriendly(..)
  , HasHitpointsHealed(..)
  , HasLongDescription(..)
  , HasMaxHitpoints(..)
  , HasName(..)
  , HasSpeed(..)
  , HasWieldable(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Data.Aeson.Generic.DerivingVia
import Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import Xanthous.Messages (Message(..))
import Xanthous.Data (TicksPerTile, Hitpoints)
import Xanthous.Data.EntityChar
import Xanthous.Util.QuickCheck
--------------------------------------------------------------------------------

data CreatureType = CreatureType
  { _name         :: !Text
  , _description  :: !Text
  , _char         :: !EntityChar
  , _maxHitpoints :: !Hitpoints
  , _friendly     :: !Bool
  , _speed        :: !TicksPerTile
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary CreatureType
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       CreatureType
makeFieldsNoPrefix ''CreatureType

hostile :: Lens' CreatureType Bool
hostile = friendly . involuted not

--------------------------------------------------------------------------------

data EdibleItem = EdibleItem
  { _hitpointsHealed :: !Int
  , _eatMessage      :: !(Maybe Message)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary EdibleItem
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       EdibleItem
makeFieldsNoPrefix ''EdibleItem

data WieldableItem = WieldableItem
  { _damage :: !Hitpoints
  , _attackMessage :: !(Maybe Message)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary WieldableItem
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       WieldableItem
makeFieldsNoPrefix ''WieldableItem

--------------------------------------------------------------------------------

data ItemType = ItemType
  { _name            :: !Text
  , _description     :: !Text
  , _longDescription :: !Text
  , _char            :: !EntityChar
  , _edible          :: !(Maybe EdibleItem)
  , _wieldable       :: !(Maybe WieldableItem)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary ItemType
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       ItemType
makeFieldsNoPrefix ''ItemType

-- | Can this item be eaten?
isEdible :: ItemType -> Bool
isEdible = has $ edible . _Just

-- | Can this item be used as a weapon?
isWieldable :: ItemType -> Bool
isWieldable = has $ wieldable . _Just

--------------------------------------------------------------------------------

data EntityRaw
  = Creature !CreatureType
  | Item !ItemType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving Arbitrary via GenericArbitrary EntityRaw
  deriving (FromJSON)
       via WithOptions '[ SumEnc ObjWithSingleField ]
                       EntityRaw
makePrisms ''EntityRaw

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Creature
  ( -- * Creature
    Creature(..)
    -- ** Lenses
  , creatureType
  , hitpoints
  , hippocampus
  , inventory

    -- ** Creature functions
  , damage
  , isDead
  , visionRadius

    -- * Hippocampus
  , Hippocampus(..)
    -- ** Lenses
  , destination
    -- ** Destination
  , Destination(..)
  , destinationFromPos
    -- *** Lenses
  , destinationPosition
  , destinationProgress
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Test.QuickCheck
import           Data.Aeson.Generic.DerivingVia
import           Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import           Xanthous.AI.Gormlak
import           Xanthous.Entities.RawTypes hiding
                 (Creature, description, damage)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Game.State
import           Xanthous.Data
import           Xanthous.Data.Entities
import           Xanthous.Entities.Creature.Hippocampus
import           Xanthous.Util.QuickCheck (GenericArbitrary(..))
import           Xanthous.Entities.Common (Inventory, HasInventory(..))
--------------------------------------------------------------------------------

data Creature = Creature
  { _creatureType   :: !CreatureType
  , _hitpoints      :: !Hitpoints
  , _hippocampus    :: !Hippocampus
  , _inventory      :: !Inventory
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Draw via DrawRawCharPriority "_creatureType" 1000 Creature
  deriving Arbitrary via GenericArbitrary Creature
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Creature
makeFieldsNoPrefix ''Creature

instance HasVisionRadius Creature where
  visionRadius = const 50 -- TODO

instance Brain Creature where
  step = brainVia GormlakBrain
  entityCanMove = const True

instance Entity Creature where
  entityAttributes _ = defaultEntityAttributes
    & blocksObject .~ True
  description = view $ creatureType . Raw.description
  entityChar = view $ creatureType . char
  entityCollision = const $ Just Combat

--------------------------------------------------------------------------------

damage :: Hitpoints -> Creature -> Creature
damage amount = hitpoints %~ \hp ->
  if hp <= amount
  then 0
  else hp - amount

isDead :: Creature -> Bool
isDead = views hitpoints (== 0)

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}

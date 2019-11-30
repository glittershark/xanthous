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

    -- ** Creature functions
  , newWithType
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
import           Test.QuickCheck.Arbitrary.Generic
import           Data.Aeson.Generic.DerivingVia
import           Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes hiding (Creature, description)
import           Xanthous.Game.State
import           Xanthous.Data
--------------------------------------------------------------------------------

data Destination = Destination
  { _destinationPosition :: !Position
    -- | The progress towards the destination, tracked as an offset from the
    -- creature's original position.
    --
    -- When this value reaches >= 1, the creature has reached their destination
  , _destinationProgress :: !Tiles
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Destination
instance Arbitrary Destination where arbitrary = genericArbitrary
makeLenses ''Destination

destinationFromPos :: Position -> Destination
destinationFromPos _destinationPosition =
  let _destinationProgress = 0
  in Destination{..}

data Hippocampus = Hippocampus
  { _destination :: !(Maybe Destination)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Hippocampus
instance Arbitrary Hippocampus where arbitrary = genericArbitrary
makeLenses ''Hippocampus

initialHippocampus :: Hippocampus
initialHippocampus = Hippocampus Nothing

--------------------------------------------------------------------------------

data Creature = Creature
  { _creatureType :: !CreatureType
  , _hitpoints    :: !Hitpoints
  , _hippocampus  :: !Hippocampus
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Draw via DrawRawCharPriority "_creatureType" 1000 Creature
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Creature
instance Arbitrary Creature where arbitrary = genericArbitrary
makeLenses ''Creature


--------------------------------------------------------------------------------

newWithType :: CreatureType -> Creature
newWithType _creatureType =
  let _hitpoints = _creatureType ^. maxHitpoints
      _hippocampus = initialHippocampus
  in Creature {..}

damage :: Hitpoints -> Creature -> Creature
damage amount = hitpoints %~ \hp ->
  if hp <= amount
  then 0
  else hp - amount

isDead :: Creature -> Bool
isDead = views hitpoints (== 0)

visionRadius :: Creature -> Word
visionRadius = const 50 -- TODO

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}

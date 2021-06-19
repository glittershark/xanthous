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
import           Control.Monad.Random (MonadRandom)
--------------------------------------------------------------------------------
import           Xanthous.AI.Gormlak
import           Xanthous.Entities.RawTypes hiding
                 (Creature, description, damage)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Game.State
import           Xanthous.Data
import           Xanthous.Data.Entities
import           Xanthous.Entities.Creature.Hippocampus
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

newWithType :: MonadRandom m => CreatureType -> m Creature
newWithType _creatureType =
  let _hitpoints = _creatureType ^. maxHitpoints
      _hippocampus = initialHippocampus
  in pure Creature {..}

damage :: Hitpoints -> Creature -> Creature
damage amount = hitpoints %~ \hp ->
  if hp <= amount
  then 0
  else hp - amount

isDead :: Creature -> Bool
isDead = views hitpoints (== 0)

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}

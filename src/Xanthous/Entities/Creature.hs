{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Creature
  ( Creature(..)
  , creatureType
  , hitpoints
  , newWithType
  , damage
  , isDead
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Test.QuickCheck.Arbitrary.Generic
import           Data.Aeson.Generic.DerivingVia
import           Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes hiding (Creature, description)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Entities (Draw(..), Entity(..), DrawRawChar(..))
--------------------------------------------------------------------------------

data Creature = Creature
  { _creatureType :: CreatureType
  , _hitpoints :: Word
  }
  deriving stock (Eq, Show, Generic)
  deriving Draw via DrawRawChar "_creatureType" Creature
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Creature
makeLenses ''Creature

instance Arbitrary Creature where
  arbitrary = genericArbitrary

instance Entity Creature where
  blocksVision _ = False
  description = view $ creatureType . Raw.description

newWithType :: CreatureType -> Creature
newWithType _creatureType =
  let _hitpoints = _creatureType ^. maxHitpoints
  in Creature {..}

damage :: Word -> Creature -> Creature
damage amount = hitpoints %~ \hp ->
  if hp <= amount
  then 0
  else hp - amount

isDead :: Creature -> Bool
isDead = views hitpoints (== 0)

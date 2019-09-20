{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Creature
  ( Creature(..)
  , creatureType
  , hitpoints
  , newWithType
  , damage
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Data.Word
import Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------
import Xanthous.Entities.RawTypes hiding (Creature)
import Xanthous.Entities (Draw(..), Entity(..), DrawRawChar(..))
--------------------------------------------------------------------------------

data Creature = Creature
  { _creatureType :: CreatureType
  , _hitpoints :: Word16
  }
  deriving stock (Eq, Show, Generic)
  deriving Draw via DrawRawChar "_creatureType" Creature
makeLenses ''Creature

instance Arbitrary Creature where
  arbitrary = genericArbitrary

instance Entity Creature where
  blocksVision _ = False

newWithType :: CreatureType -> Creature
newWithType _creatureType =
  let _hitpoints = _creatureType ^. maxHitpoints
  in Creature {..}

damage :: Word16 -> Creature -> Creature
damage amount = hitpoints %~ \hp ->
  if hp <= amount
  then 0
  else hp - amount

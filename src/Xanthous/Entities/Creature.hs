{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Xanthous.Entities.Creature where

import Data.Word

import Xanthous.Prelude
import Xanthous.Entities.RawTypes hiding (Creature)
import Xanthous.Entities (Draw(..))

data Creature = Creature
  { _creatureType :: CreatureType
  , _hitpoints :: Word16
  }
  deriving stock (Eq, Show, Generic)
makeLenses ''Creature

instance Draw Creature where
  draw = draw .view (creatureType . char)

newWithType :: CreatureType -> Creature
newWithType _creatureType =
  let _hitpoints = _creatureType ^. maxHitpoints
  in Creature {..}

damage :: Word16 -> Creature -> Creature
damage amount = hitpoints %~ \hp ->
  if hp <= amount
  then 0
  else hp - amount

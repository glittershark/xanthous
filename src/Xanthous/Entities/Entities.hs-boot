{-# OPTIONS_GHC -fno-warn-orphans #-}
module Xanthous.Entities.Entities where

import Test.QuickCheck
import Data.Aeson
import Xanthous.Game.State (SomeEntity, GameState, Entity)

instance Arbitrary SomeEntity
instance Function SomeEntity
instance CoArbitrary SomeEntity
instance FromJSON SomeEntity
instance Entity SomeEntity

instance FromJSON GameState

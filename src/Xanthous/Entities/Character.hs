module Xanthous.Entities.Character
  ( Character(..)
  , mkCharacter
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
--------------------------------------------------------------------------------
import Xanthous.Entities
--------------------------------------------------------------------------------

data Character = Character
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (CoArbitrary, Function)
  deriving Draw via (DrawCharacter "@" Character)

instance Arbitrary Character where
  arbitrary = pure Character

mkCharacter :: Character
mkCharacter = Character

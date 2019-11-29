--------------------------------------------------------------------------------
module Xanthous.Resource
  ( Name(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------

data Name = MapViewport
            -- ^ The main viewport where we display the game content
          | Character
            -- ^ The character
          | MessageBox
            -- ^ The box where we display messages to the user
          | Prompt
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Arbitrary Name where
  arbitrary = genericArbitrary

module Xanthous.Entities.Character
  ( Character(..)
  , mkCharacter
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Brick
--------------------------------------------------------------------------------
import Xanthous.Entities
--------------------------------------------------------------------------------

data Character = Character
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (CoArbitrary, Function)

scrollOffset :: Int
scrollOffset = 5

-- deriving Draw via (DrawCharacter "@" Character)
instance Draw Character where
  draw _ = visibleRegion rloc rreg $ str "@"
    where
      rloc = Location (negate scrollOffset, negate scrollOffset)
      rreg = (2 * scrollOffset, 2 * scrollOffset)

instance Entity Character where
  blocksVision _ = False

instance Arbitrary Character where
  arbitrary = pure Character

mkCharacter :: Character
mkCharacter = Character

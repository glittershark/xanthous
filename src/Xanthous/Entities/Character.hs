{-# LANGUAGE TemplateHaskell #-}
module Xanthous.Entities.Character
  ( Character(..)
  , mkCharacter
  , pickUpItem
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Arbitrary.Generic
import Brick
--------------------------------------------------------------------------------
import Xanthous.Entities
import Xanthous.Entities.Item
--------------------------------------------------------------------------------

data Character = Character
  { _inventory :: !(Vector Item)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (CoArbitrary, Function)
makeLenses ''Character

scrollOffset :: Int
scrollOffset = 5

instance Draw Character where
  draw _ = visibleRegion rloc rreg $ str "@"
    where
      rloc = Location (negate scrollOffset, negate scrollOffset)
      rreg = (2 * scrollOffset, 2 * scrollOffset)

instance Entity Character where
  blocksVision _ = False

instance Arbitrary Character where
  arbitrary = genericArbitrary

mkCharacter :: Character
mkCharacter = Character
  { _inventory = mempty
  }

pickUpItem :: Item -> Character -> Character
pickUpItem item = inventory %~ (item <|)


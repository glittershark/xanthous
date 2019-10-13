{-# LANGUAGE TemplateHaskell #-}
module Xanthous.Entities.Character
  ( Character(..)
  , characterName
  , inventory
  , characterDamage
  , characterHitpoints
  , speed

    -- *
  , mkCharacter
  , pickUpItem
  , isDead
  , damage
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Test.QuickCheck
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Arbitrary.Generic
import Brick
import Data.Aeson.Generic.DerivingVia
import Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import Xanthous.Entities
import Xanthous.Entities.Item
import Xanthous.Data (TicksPerTile)
--------------------------------------------------------------------------------

data Character = Character
  { _inventory :: !(Vector Item)
  , _characterName :: !(Maybe Text)
  , _characterDamage :: !Word
  , _characterHitpoints :: !Word
  , _speed :: TicksPerTile
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           Character
makeLenses ''Character

scrollOffset :: Int
scrollOffset = 5

instance Draw Character where
  draw _ = visibleRegion rloc rreg $ str "@"
    where
      rloc = Location (negate scrollOffset, negate scrollOffset)
      rreg = (2 * scrollOffset, 2 * scrollOffset)

-- the character does not (yet) have a mind of its own
instance Brain Character where step = brainVia Brainless

instance Entity Character where
  blocksVision _ = False
  description _ = "yourself"

instance Arbitrary Character where
  arbitrary = genericArbitrary

initialHitpoints :: Word
initialHitpoints = 10

defaultSpeed :: TicksPerTile
defaultSpeed = 100

mkCharacter :: Character
mkCharacter = Character
  { _inventory = mempty
  , _characterName = Nothing
  , _characterDamage = 1
  , _characterHitpoints = initialHitpoints
  , _speed = defaultSpeed
  }

isDead :: Character -> Bool
isDead = (== 0) . view characterHitpoints

pickUpItem :: Item -> Character -> Character
pickUpItem item = inventory %~ (item <|)

damage :: Word -> Character -> Character
damage amount = characterHitpoints %~ \case
  n | n <= amount -> 0
    | otherwise  -> n - amount

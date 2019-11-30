{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Xanthous.Entities.Character
  ( Character(..)
  , characterName
  , inventory
  , characterDamage
  , characterHitpoints'
  , characterHitpoints
  , hitpointRecoveryRate
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
import Data.Coerce (coerce)
--------------------------------------------------------------------------------
import Xanthous.Game.State
import Xanthous.Entities.Item
import Xanthous.Data (TicksPerTile, Hitpoints, Per, Ticks, (|*|), positioned)
--------------------------------------------------------------------------------

data Character = Character
  { _inventory :: !(Vector Item)
  , _characterName :: !(Maybe Text)
  , _characterDamage :: !Hitpoints
  , _characterHitpoints' :: !Double
  , _speed :: TicksPerTile
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           Character
makeLenses ''Character

characterHitpoints :: Character -> Hitpoints
characterHitpoints = views characterHitpoints' floor

scrollOffset :: Int
scrollOffset = 5

instance Draw Character where
  draw _ = visibleRegion rloc rreg $ str "@"
    where
      rloc = Location (negate scrollOffset, negate scrollOffset)
      rreg = (2 * scrollOffset, 2 * scrollOffset)
  drawPriority = const maxBound -- Character should always be on top, for now

instance Brain Character where
  step ticks = (pure .) $ positioned . characterHitpoints' %~ \hp ->
    if hp > fromIntegral initialHitpoints
    then hp
    else hp + hitpointRecoveryRate |*| ticks

instance Entity Character where
  blocksVision _ = False
  description _ = "yourself"
  entityChar _ = "@"

instance Arbitrary Character where
  arbitrary = genericArbitrary

initialHitpoints :: Hitpoints
initialHitpoints = 10

hitpointRecoveryRate :: Double `Per` Ticks
hitpointRecoveryRate = 1.0 / (15 * coerce defaultSpeed)

defaultSpeed :: TicksPerTile
defaultSpeed = 100

mkCharacter :: Character
mkCharacter = Character
  { _inventory = mempty
  , _characterName = Nothing
  , _characterDamage = 1
  , _characterHitpoints' = fromIntegral initialHitpoints
  , _speed = defaultSpeed
  }

isDead :: Character -> Bool
isDead = (== 0) . characterHitpoints

pickUpItem :: Item -> Character -> Character
pickUpItem item = inventory %~ (item <|)

damage :: Hitpoints -> Character -> Character
damage (fromIntegral -> amount) = characterHitpoints' %~ \case
  n | n <= amount -> 0
    | otherwise  -> n - amount

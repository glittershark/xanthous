{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Character

  ( -- * Character datatype
    Character(..)
  , characterName
  , HasInventory(..)
  , characterDamage
  , characterHitpoints'
  , characterHitpoints
  , hitpointRecoveryRate
  , speed
  , body

    -- *** Body
  , Body(..)
  , initialBody
  , knuckles
  , Knuckles(..)
  , fistDamageChance
  , damageKnuckles
  , fistfightingDamage

    -- * Character functions
  , mkCharacter
  , pickUpItem
  , isDead
  , isFullyHealed
  , damage
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import           Brick
import           Data.Aeson.Generic.DerivingVia
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Coerce (coerce)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Vector ()
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Gen (chooseUpTo)
import           Test.QuickCheck.Checkers (EqProp)
import           Control.Monad.State.Lazy (execState)
import           Control.Monad.Trans.State.Lazy (execStateT)
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Entities.Item
import           Xanthous.Entities.Common
import           Xanthous.Data
                 ( TicksPerTile, Hitpoints, Per, Ticks, (|*|), positioned )
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Util (EqEqProp(EqEqProp), modifyKL)
import           Xanthous.Monad (say_)
--------------------------------------------------------------------------------

-- | The status of the character's knuckles
--
-- This struct is used to track the damage and then eventual build-up of
-- calluses when the character is fighting with their fists
data Knuckles = Knuckles
  { -- | How damaged are the knuckles currently, from 0 to 5?
    --
    -- At 0, no calluses will form
    -- At 1 and up, the character will form calluses after a while
    -- At 5, continuing to fistfight will deal the character even more damage
    _knuckleDamage   :: !Word
    -- | How built-up are the character's calluses, from 0 to 5?
    --
    -- Each level of calluses decreases the likelihood of being damaged when
    -- fistfighting by 1%, up to 5 where the character will never be damaged
    -- fistfighting
  , _knuckleCalluses :: !Word

    -- | Number of turns that have passed since the last time the knuckles were
    -- damaged
  , _ticksSinceDamaged :: Ticks
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving EqProp via EqEqProp Knuckles
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           Knuckles
makeLenses ''Knuckles

instance Semigroup Knuckles where
  (Knuckles d₁ c₁ t₁) <> (Knuckles d₂ c₂ t₂) = Knuckles
    (min (d₁ + d₂) 5)
    (min (c₁ + c₂) 5)
    (max t₁ t₂)

instance Monoid Knuckles where
  mempty = Knuckles 0 0 0

instance Arbitrary Knuckles where
  arbitrary = do
    _knuckleDamage <- fromIntegral <$> chooseUpTo 5
    _knuckleCalluses <- fromIntegral <$> chooseUpTo 5
    _ticksSinceDamaged <- arbitrary
    pure Knuckles{..}

-- | Likelihood that the character fighting with their fists will damage
-- themselves
fistDamageChance :: Knuckles -> Float
fistDamageChance knuckles
  | calluses == 5 = 0
  | otherwise = baseChance - (0.01 * fromIntegral calluses)
  where
    baseChance = 0.08
    calluses = knuckles ^. knuckleCalluses

-- | Damage the knuckles by a level (capping at the max knuckle damage)
damageKnuckles :: Knuckles -> Knuckles
damageKnuckles = execState $ do
  knuckleDamage %= min 5 . succ
  ticksSinceDamaged .= 0

-- | Damage taken when fistfighting and 'fistDamageChance' has occurred
fistfightingDamage :: Knuckles -> Hitpoints
fistfightingDamage knuckles
  | knuckles ^. knuckleDamage == 5 = 2
  | otherwise = 1

stepKnuckles :: Ticks -> Knuckles -> AppM Knuckles
stepKnuckles ticks = execStateT . whenM (uses knuckleDamage (> 0)) $ do
  ticksSinceDamaged += ticks
  whenM (uses ticksSinceDamaged (>= 2000)) $ do
    dam <- knuckleDamage <<.= 0
    knuckleCalluses %= min 5 . (+ dam)
    ticksSinceDamaged .= 0
    lift $ say_ ["character", "body", "knuckles", "calluses"]


-- | Status of the character's body
data Body = Body
  { _knuckles :: !Knuckles
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Body
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           Body
makeLenses ''Body

initialBody :: Body
initialBody = Body { _knuckles = mempty }

--------------------------------------------------------------------------------

data Character = Character
  { _inventory           :: !Inventory
  , _characterName       :: !(Maybe Text)
  , _characterHitpoints' :: !Double
  , _speed               :: !TicksPerTile
  , _body                :: !Body
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           Character
makeFieldsNoPrefix ''Character

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
  step ticks = execStateT $ do
    positioned . characterHitpoints' %= \hp ->
      if hp > fromIntegral initialHitpoints
      then hp
      else hp + hitpointRecoveryRate |*| ticks
    modifyKL (positioned . body . knuckles) $ lift . stepKnuckles ticks

instance Entity Character where
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
  { _inventory           = mempty
  , _characterName       = Nothing
  , _characterHitpoints' = fromIntegral initialHitpoints
  , _speed               = defaultSpeed
  , _body                = initialBody
  }

defaultCharacterDamage :: Hitpoints
defaultCharacterDamage = 1

-- | Returns the damage that the character currently does with an attack
-- TODO use double-handed/left-hand/right-hand here
characterDamage :: Character -> Hitpoints
characterDamage
  = fromMaybe defaultCharacterDamage
  . filter (/= 0)
  . Just
  . sumOf (inventory . wielded . wieldedItems . wieldableItem . Raw.damage)

-- | Is the character fully healed up to or past their initial hitpoints?
isFullyHealed :: Character -> Bool
isFullyHealed = (>= initialHitpoints) . characterHitpoints

-- | Is the character dead?
isDead :: Character -> Bool
isDead = (== 0) . characterHitpoints

pickUpItem :: Item -> Character -> Character
pickUpItem it = inventory . backpack %~ (it <|)

damage :: Hitpoints -> Character -> Character
damage (fromIntegral -> amount) = characterHitpoints' %~ \case
  n | n <= amount -> 0
    | otherwise  -> n - amount

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}

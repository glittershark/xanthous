{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Character

  ( -- * Character datatype
    Character(..)
  , characterName
  , inventory
  , characterDamage
  , characterHitpoints'
  , characterHitpoints
  , hitpointRecoveryRate
  , speed
  , body

    -- ** Inventory
  , Inventory(..)
  , backpack
  , wielded
  , items
    -- *** Wielded items
  , Wielded(..)
  , hands
  , leftHand
  , rightHand
  , inLeftHand
  , inRightHand
  , doubleHanded
  , wieldedItems
  , WieldedItem(..)
  , wieldedItem
  , wieldableItem
  , asWieldedItem

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
--------------------------------------------------------------------------------
import           Xanthous.Util.QuickCheck
import           Xanthous.Game.State
import           Xanthous.Entities.Item
import           Xanthous.Data
                 ( TicksPerTile, Hitpoints, Per, Ticks, (|*|), positioned
                 , Positioned(..)
                 )
import           Xanthous.Entities.RawTypes (WieldableItem, wieldable)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Util (EqEqProp(EqEqProp), modifyKL)
import Control.Monad.State.Lazy (execState)
import Control.Monad.Trans.State.Lazy (execStateT)
import Xanthous.Monad (say_)
--------------------------------------------------------------------------------

data WieldedItem = WieldedItem
  { _wieldedItem :: Item
  , _wieldableItem :: WieldableItem
    -- ^ Invariant: item ^. itemType . wieldable ≡ Just wieldableItem
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           WieldedItem
makeFieldsNoPrefix ''WieldedItem

asWieldedItem :: Prism' Item WieldedItem
asWieldedItem = prism' hither yon
 where
   yon item = WieldedItem item <$> item ^. itemType . wieldable
   hither (WieldedItem item _) = item

instance Brain WieldedItem where
  step ticks (Positioned p wi) =
    over positioned (\i -> WieldedItem i $ wi ^. wieldableItem)
    <$> step ticks (Positioned p $ wi ^. wieldedItem)

instance Draw WieldedItem where
  draw = draw . view wieldedItem

instance Entity WieldedItem where
  entityAttributes = entityAttributes . view wieldedItem
  description = description . view wieldedItem
  entityChar = entityChar . view wieldedItem

instance Arbitrary WieldedItem where
  arbitrary = genericArbitrary <&> \wi ->
    wi & wieldedItem . itemType . wieldable ?~ wi ^. wieldableItem

data Wielded
  = DoubleHanded WieldedItem
  | Hands { _leftHand :: !(Maybe WieldedItem)
          , _rightHand :: !(Maybe WieldedItem)
          }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Wielded
  deriving (ToJSON, FromJSON)
       via WithOptions '[ 'SumEnc 'ObjWithSingleField ]
           Wielded

hands :: Prism' Wielded (Maybe WieldedItem, Maybe WieldedItem)
hands = prism' (uncurry Hands) $ \case
  Hands l r -> Just (l, r)
  _ -> Nothing

leftHand :: Traversal' Wielded WieldedItem
leftHand = hands . _1 . _Just

inLeftHand :: WieldedItem -> Wielded
inLeftHand wi = Hands (Just wi) Nothing

rightHand :: Traversal' Wielded WieldedItem
rightHand = hands . _2 . _Just

inRightHand :: WieldedItem -> Wielded
inRightHand wi = Hands Nothing (Just wi)

doubleHanded :: Prism' Wielded WieldedItem
doubleHanded = prism' DoubleHanded $ \case
  DoubleHanded i -> Just i
  _ -> Nothing

wieldedItems :: Traversal' Wielded WieldedItem
wieldedItems k (DoubleHanded wielded) = DoubleHanded <$> k wielded
wieldedItems k (Hands l r) = Hands <$> _Just k l <*> _Just k r

data Inventory = Inventory
  { _backpack :: Vector Item
  , _wielded :: Wielded
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Inventory
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           Inventory
makeFieldsNoPrefix ''Inventory

items :: Traversal' Inventory Item
items k (Inventory bp w) = Inventory
  <$> traversed k bp
  <*> (wieldedItems . wieldedItem) k w

type instance Element Inventory = Item

instance MonoFunctor Inventory where
  omap = over items

instance MonoFoldable Inventory where
  ofoldMap = foldMapOf items
  ofoldr = foldrOf items
  ofoldl' = foldlOf' items
  otoList = toListOf items
  oall = allOf items
  oany = anyOf items
  onull = nullOf items
  ofoldr1Ex = foldr1Of items
  ofoldl1Ex' = foldl1Of' items
  headEx = headEx . toListOf items
  lastEx = lastEx . toListOf items

instance MonoTraversable Inventory where
  otraverse = traverseOf items

instance Semigroup Inventory where
  inv₁ <> inv₂ =
    let backpack' = inv₁ ^. backpack <> inv₂ ^. backpack
        (wielded', backpack'') = case (inv₁ ^. wielded, inv₂ ^. wielded) of
          (wielded₁, wielded₂@(DoubleHanded _)) ->
            (wielded₂, backpack' <> fromList (wielded₁ ^.. wieldedItems . wieldedItem))
          (wielded₁, wielded₂@(Hands (Just _) (Just _))) ->
            (wielded₂, backpack' <> fromList (wielded₁ ^.. wieldedItems . wieldedItem))
          (wielded₁, Hands Nothing Nothing) -> (wielded₁, backpack')
          (Hands Nothing Nothing, wielded₂) -> (wielded₂, backpack')
          (Hands (Just l₁) Nothing, Hands Nothing (Just r₂)) ->
            (Hands (Just l₁) (Just r₂), backpack')
          (wielded₁@(DoubleHanded _), wielded₂) ->
            (wielded₁, backpack' <> fromList (wielded₂ ^.. wieldedItems . wieldedItem))
          (Hands Nothing (Just r₁), Hands Nothing (Just r₂)) ->
            (Hands Nothing (Just r₂), r₁ ^. wieldedItem <| backpack')
          (Hands Nothing r₁, Hands (Just l₂) Nothing) ->
            (Hands (Just l₂) r₁, backpack')
          (Hands (Just l₁) Nothing, Hands (Just l₂) Nothing) ->
            (Hands (Just l₂) Nothing, l₁ ^. wieldedItem <| backpack')
          (Hands (Just l₁) (Just r₁), Hands Nothing (Just r₂)) ->
            (Hands (Just l₁) (Just r₂), r₁ ^. wieldedItem <| backpack')
          (Hands (Just l₁) (Just r₁), Hands (Just l₂) Nothing) ->
            (Hands (Just l₂) (Just r₁), l₁ ^. wieldedItem <| backpack')
    in Inventory backpack'' wielded'

instance Monoid Inventory where
  mempty = Inventory mempty $ Hands Nothing Nothing

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
  . preview (inventory . wielded . wieldedItems . wieldableItem . Raw.damage)

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

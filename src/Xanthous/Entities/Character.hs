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

    -- * Inventory
  , Inventory(..)
  , backpack
  , wielded
  , items
    -- ** Wielded items
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

    -- *
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

data Character = Character
  { _inventory :: !Inventory
  , _characterName :: !(Maybe Text)
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
  , _characterHitpoints' = fromIntegral initialHitpoints
  , _speed = defaultSpeed
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

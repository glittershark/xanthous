{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Xanthous.Entities.Common
-- Description : Common data type definitions and utilities for entities
--
--------------------------------------------------------------------------------
module Xanthous.Entities.Common
  ( -- * Inventory
    Inventory(..)
  , HasInventory(..)
  , backpack
  , wielded
  , items
  , InventoryPosition(..)
  , describeInventoryPosition
  , inventoryPosition
  , itemsWithPosition
  , removeItemFromPosition

    -- ** Wielded items
  , Wielded(..)
  , nothingWielded
  , hands
  , leftHand
  , rightHand
  , inLeftHand
  , inRightHand
  , doubleHanded
  , Hand(..)
  , itemsInHand
  , inHand
  , wieldInHand
  , describeHand
  , wieldedItems
  , WieldedItem(..)
  , wieldedItem
  , wieldableItem
  , asWieldedItem
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Aeson.Generic.DerivingVia
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp)
--------------------------------------------------------------------------------
import           Xanthous.Data (Positioned(..), positioned)
import           Xanthous.Util.QuickCheck
import           Xanthous.Game.State
import           Xanthous.Entities.Item
import           Xanthous.Entities.RawTypes (WieldableItem, wieldable)
import           Xanthous.Util (removeFirst, EqEqProp(..))
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


nothingWielded :: Wielded
nothingWielded = Hands Nothing Nothing

hands :: Prism' Wielded (Maybe WieldedItem, Maybe WieldedItem)
hands = prism' (uncurry Hands) $ \case
  Hands l r -> Just (l, r)
  _ -> Nothing

leftHand :: Traversal' Wielded (Maybe WieldedItem)
leftHand = hands . _1

inLeftHand :: WieldedItem -> Wielded
inLeftHand wi = Hands (Just wi) Nothing

rightHand :: Traversal' Wielded (Maybe WieldedItem)
rightHand = hands . _2

inRightHand :: WieldedItem -> Wielded
inRightHand wi = Hands Nothing (Just wi)

doubleHanded :: Prism' Wielded WieldedItem
doubleHanded = prism' DoubleHanded $ \case
  DoubleHanded i -> Just i
  _ -> Nothing

wieldedItems :: Traversal' Wielded WieldedItem
wieldedItems k (DoubleHanded wielded) = DoubleHanded <$> k wielded
wieldedItems k (Hands l r) = Hands <$> _Just k l <*> _Just k r


data Hand
  = LeftHand
  | RightHand
  | BothHands
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Hand

itemsInHand :: Hand -> Wielded -> [WieldedItem]
itemsInHand LeftHand (DoubleHanded wi) = [wi]
itemsInHand LeftHand (Hands lh _) = toList lh
itemsInHand RightHand (DoubleHanded wi) = [wi]
itemsInHand RightHand (Hands _ rh) = toList rh
itemsInHand BothHands (DoubleHanded wi) = [wi]
itemsInHand BothHands (Hands lh rh) = toList lh <> toList rh

inHand :: Hand -> WieldedItem -> Wielded
inHand LeftHand = inLeftHand
inHand RightHand = inRightHand
inHand BothHands = review doubleHanded

wieldInHand :: Hand -> WieldedItem -> Wielded -> ([WieldedItem], Wielded)
wieldInHand hand item w = (itemsInHand hand w, doWield)
  where
    doWield = case (hand, w) of
      (LeftHand, Hands _ r) -> Hands (Just item) r
      (LeftHand, DoubleHanded _) -> inLeftHand item
      (RightHand, Hands l _) -> Hands l (Just item)
      (RightHand, DoubleHanded _) -> inRightHand item
      (BothHands, _) -> DoubleHanded item

describeHand :: Hand -> Text
describeHand LeftHand = "your left hand"
describeHand RightHand = "your right hand"
describeHand BothHands = "both hands"

data Inventory = Inventory
  { _backpack :: Vector Item
  , _wielded :: Wielded
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Inventory
  deriving EqProp via EqEqProp Inventory
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

class HasInventory s a | s -> a where
  inventory :: Lens' s a
  {-# MINIMAL inventory #-}

-- | Representation for where in the inventory an item might be
data InventoryPosition
  = Backpack
  | InHand Hand
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary InventoryPosition

-- | Return a human-readable description of the given 'InventoryPosition'
describeInventoryPosition :: InventoryPosition -> Text
describeInventoryPosition Backpack       = "In backpack"
describeInventoryPosition (InHand hand)  = "Wielded, in " <> describeHand hand

-- | Given a position in the inventory, return a traversal on the inventory over
-- all the items in that position
inventoryPosition :: InventoryPosition -> Traversal' Inventory Item
inventoryPosition Backpack = backpack . traversed
inventoryPosition (InHand LeftHand) = wielded . leftHand . _Just . wieldedItem
inventoryPosition (InHand RightHand) = wielded . leftHand . _Just . wieldedItem
inventoryPosition (InHand BothHands) = wielded . doubleHanded . wieldedItem

-- | A fold over all the items in the inventory accompanied by their position in
-- the inventory
--
-- Invariant: This will return items in the same order as 'items'
itemsWithPosition :: Fold Inventory (InventoryPosition, Item)
itemsWithPosition = folding $ (<>) <$> backpackItems <*> handItems
  where
    backpackItems = toListOf $ backpack . folded . to (Backpack ,)
    handItems inv = case inv ^. wielded of
       DoubleHanded i -> pure (InHand BothHands, i ^. wieldedItem)
       Hands l r -> (l ^.. folded . wieldedItem . to (InHand LeftHand ,))
                 <> (r ^.. folded . wieldedItem . to (InHand RightHand ,))

-- | Remove the first item equal to 'Item' from the given position in the
-- inventory
removeItemFromPosition :: InventoryPosition -> Item -> Inventory -> Inventory
removeItemFromPosition Backpack item inv
  = inv & backpack %~ removeFirst (== item)
removeItemFromPosition (InHand LeftHand) item inv
  = inv & wielded . leftHand %~ filter ((/= item) . view wieldedItem)
removeItemFromPosition (InHand RightHand) item inv
  = inv & wielded . rightHand %~ filter ((/= item) . view wieldedItem)
removeItemFromPosition (InHand BothHands) item inv
  | has (wielded . doubleHanded . wieldedItem . filtered (== item)) inv
  = inv & wielded .~ nothingWielded
  | otherwise
  = inv

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Xanthous.Data.EntityMap
  ( EntityMap
  , EntityID
  , emptyEntityMap
  , insertAt
  , insertAtReturningID
  , atPosition
  , positions
  , lookup
  , lookupWithPosition
  -- , positionedEntities
  , neighbors
  ) where

import Data.Monoid (Endo(..))
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Checkers (EqProp)

import Xanthous.Prelude hiding (lookup)
import Xanthous.Data
  ( Position
  , Positioned(..)
  , positioned
  , position
  , Neighbors(..)
  , neighborPositions
  )
import Xanthous.Orphans ()
import Xanthous.Util (EqEqProp(..))

type EntityID = Word32
type NonNullVector a = NonNull (Vector a)

data EntityMap a where
  EntityMap ::
    { _byPosition :: Map Position (NonNullVector EntityID)
    , _byID       :: HashMap EntityID (Positioned a)
    , _lastID     :: EntityID
    } -> EntityMap a
  deriving stock (Functor, Foldable, Traversable)
deriving via (EqEqProp (EntityMap a)) instance Eq a => EqProp (EntityMap a)
makeLenses ''EntityMap

byIDInvariantError :: forall a. a
byIDInvariantError = error $ "Invariant violation: All EntityIDs in byPosition "
  <> "must point to entityIDs in byID"

instance Eq a => Eq (EntityMap a) where
  em₁ == em₂ = em₁ ^. _EntityMap == em₂ ^. _EntityMap

instance Show a => Show (EntityMap a) where
  show em = "_EntityMap # " <> show (em ^. _EntityMap)

instance Arbitrary a => Arbitrary (EntityMap a) where
  arbitrary = review _EntityMap <$> arbitrary

type instance Index (EntityMap a) = EntityID
type instance IxValue (EntityMap a) = (Positioned a)
instance Ixed (EntityMap a) where ix eid = at eid . traverse

instance At (EntityMap a) where
  at eid = lens (view $ byID . at eid) setter
    where
      setter :: EntityMap a -> Maybe (Positioned a) -> EntityMap a
      setter m Nothing = fromMaybe m $ do
        Positioned pos _ <- m ^. byID . at eid
        pure $ m
          & removeEIDAtPos pos
          & byID . at eid .~ Nothing
      setter m (Just (Positioned pos e)) =
        case lookupWithPosition eid m of
          Nothing -> insertAt pos e m
          Just (Positioned origPos _) -> m
            & removeEIDAtPos origPos
            & byID . ix eid . position .~ pos
            & byPosition . at pos %~ \case
              Nothing -> Just $ ncons eid mempty
              Just es -> Just $ eid <| es
      removeEIDAtPos pos =
        byPosition . at pos %~ (>>= fromNullable . nfilter (/= eid))

emptyEntityMap :: EntityMap a
emptyEntityMap = EntityMap mempty mempty 0

_EntityMap :: Iso' (EntityMap a) [(Position, a)]
_EntityMap = iso hither yon
  where
    hither :: EntityMap a -> [(Position, a)]
    hither em = do
       (pos, eids) <- em ^. byPosition . _Wrapped
       eid <- toList eids
       ent <- em ^.. byID . at eid . folded . positioned
       pure (pos, ent)
    yon :: [(Position, a)] -> EntityMap a
    yon poses = alaf Endo foldMap (uncurry insertAt) poses emptyEntityMap

instance Semigroup (EntityMap a) where
  em₁ <> em₂ = alaf Endo foldMap (uncurry insertAt) (em₁ ^. _EntityMap) em₂

instance Monoid (EntityMap a) where
  mempty = emptyEntityMap


insertAtReturningID :: forall a. Position -> a -> EntityMap a -> (EntityID, EntityMap a)
insertAtReturningID pos e em =
  let (eid, em') = em & lastID <+~ 1
  in em'
     & byID . at eid ?~ Positioned pos e
     & byPosition . at pos %~ \case
       Nothing -> Just $ ncons eid mempty
       Just es -> Just $ eid <| es
     & (eid, )

insertAt :: forall a. Position -> a -> EntityMap a -> EntityMap a
insertAt pos e = snd . insertAtReturningID pos e

atPosition :: forall a. Position -> Lens' (EntityMap a) (Vector a)
atPosition pos = lens getter setter
  where
    getter em =
      let
        eids :: Vector EntityID
        eids = maybe mempty toNullable $ em ^. byPosition . at pos

        getEIDAssume :: EntityID -> a
        getEIDAssume eid = fromMaybe byIDInvariantError
          $ em ^? byID . ix eid . positioned
      in getEIDAssume <$> eids
    setter em Empty = em & byPosition . at pos .~ Nothing
    setter em entities = alaf Endo foldMap (insertAt pos) entities em

positions :: EntityMap a -> [Position]
positions = toListOf $ byPosition . to keys . folded

lookupWithPosition :: EntityID -> EntityMap a -> Maybe (Positioned a)
lookupWithPosition eid = view $ byID . at eid

lookup :: EntityID -> EntityMap a -> Maybe a
lookup eid = fmap (view positioned) . lookupWithPosition eid

-- unlawful :(
-- positionedEntities :: IndexedTraversal EntityID (EntityMap a) (EntityMap b) (Positioned a) (Positioned b)
-- positionedEntities = byID . itraversed

neighbors :: Position -> EntityMap a -> Neighbors (Vector a)
neighbors pos em = (\p -> view (atPosition p) em) <$> neighborPositions pos

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityMap
  ( EntityMap
  , _EntityMap
  , EntityID
  , emptyEntityMap
  , insertAt
  , insertAtReturningID
  , fromEIDsAndPositioned
  , toEIDsAndPositioned
  , atPosition
  , atPositionWithIDs
  , positions
  , lookup
  , lookupWithPosition
  -- , positionedEntities
  , neighbors
  , Deduplicate(..)

  -- * debug
  , byID
  , byPosition
  , lastID

  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude hiding (lookup)
import Xanthous.Data
  ( Position
  , Positioned(..)
  , positioned
  , Neighbors(..)
  , neighborPositions
  )
import Xanthous.Data.VectorBag
import Xanthous.Orphans ()
import Xanthous.Util (EqEqProp(..))
--------------------------------------------------------------------------------
import Data.Monoid (Endo(..))
import Test.QuickCheck (Arbitrary(..), CoArbitrary, Function)
import Test.QuickCheck.Checkers (EqProp)
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.QuickCheck.Instances.Vector ()
import Text.Show (showString, showParen)
import Data.Aeson
--------------------------------------------------------------------------------

type EntityID = Word32
type NonNullSet a = NonNull (Set a)

data EntityMap a where
  EntityMap ::
    { _byPosition :: Map Position (NonNullSet EntityID)
    , _byID       :: HashMap EntityID (Positioned a)
    , _lastID     :: EntityID
    } -> EntityMap a
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
deriving via (EqEqProp (EntityMap a)) instance (Eq a, Ord a) => EqProp (EntityMap a)
makeLenses ''EntityMap

instance ToJSON a => ToJSON (EntityMap a) where
  toJSON = toJSON . toEIDsAndPositioned


instance FromJSON a => FromJSON (EntityMap a) where
  parseJSON = fmap (fromEIDsAndPositioned @[_]) . parseJSON

byIDInvariantError :: forall a. a
byIDInvariantError = error $ "Invariant violation: All EntityIDs in byPosition "
  <> "must point to entityIDs in byID"

instance (Ord a, Eq a) => Eq (EntityMap a) where
  -- em₁ == em₂ = em₁ ^. _EntityMap == em₂ ^. _EntityMap
  (==) = (==) `on` view (_EntityMap . to sort)

deriving stock instance (Ord a) => Ord (EntityMap a)

instance Show a => Show (EntityMap a) where
  showsPrec pr em
    = showParen (pr > 10)
    $ showString
    . ("fromEIDsAndPositioned " <>)
    . show
    . toEIDsAndPositioned
    $ em

instance Arbitrary a => Arbitrary (EntityMap a) where
  arbitrary = review _EntityMap <$> arbitrary
  shrink em = review _EntityMap <$> shrink (em ^. _EntityMap)

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
      setter m (Just pe@(Positioned pos _)) = m
        & (case lookupWithPosition eid m of
             Nothing -> id
             Just (Positioned origPos _) -> removeEIDAtPos origPos
          )
        & byID . at eid ?~ pe
        & byPosition . at pos %~ \case
            Nothing -> Just $ opoint eid
            Just es -> Just $ ninsertSet eid es
      removeEIDAtPos pos =
        byPosition . at pos %~ (>>= fromNullable . ndeleteSet eid)

instance Semigroup (EntityMap a) where
  em₁ <> em₂ = alaf Endo foldMap (uncurry insertAt) (em₂ ^. _EntityMap) em₁

instance Monoid (EntityMap a) where
  mempty = emptyEntityMap

instance FunctorWithIndex EntityID EntityMap

instance FoldableWithIndex EntityID EntityMap

instance TraversableWithIndex EntityID EntityMap where
  itraversed = byID . itraversed . rmap sequenceA . distrib
  itraverse = itraverseOf itraversed

type instance Element (EntityMap a) = a
instance MonoFoldable (EntityMap a)

emptyEntityMap :: EntityMap a
emptyEntityMap = EntityMap mempty mempty 0

newtype Deduplicate a = Deduplicate (EntityMap a)
  deriving stock (Show, Traversable, Generic)
  deriving newtype (Eq, Functor, Foldable, EqProp, Arbitrary)

instance Semigroup (Deduplicate a) where
  (Deduplicate em₁) <> (Deduplicate em₂) =
    let _byID = em₁ ^. byID <> em₂ ^. byID
        _byPosition = mempty &~ do
          ifor_ _byID $ \eid (Positioned pos _) ->
            at pos %= \case
              Just eids -> Just $ ninsertSet eid eids
              Nothing -> Just $ opoint eid
        _lastID = fromMaybe 1 $ maximumOf (ifolded . asIndex) _byID
    in Deduplicate EntityMap{..}


--------------------------------------------------------------------------------

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


insertAtReturningID :: forall a. Position -> a -> EntityMap a -> (EntityID, EntityMap a)
insertAtReturningID pos e em =
  let (eid, em') = em & lastID <+~ 1
  in em'
     & byID . at eid ?~ Positioned pos e
     & byPosition . at pos %~ \case
       Nothing -> Just $ opoint eid
       Just es -> Just $ ninsertSet eid es
     & (eid, )

insertAt :: forall a. Position -> a -> EntityMap a -> EntityMap a
insertAt pos e = snd . insertAtReturningID pos e

atPosition :: forall a. (Ord a, Show a) => Position -> Lens' (EntityMap a) (VectorBag a)
atPosition pos = lens getter setter
  where
    getter em =
      let eids :: VectorBag EntityID
          eids = maybe mempty (VectorBag . toVector . toNullable)
                 $ em ^. byPosition . at pos
      in getEIDAssume em <$> eids
    setter em Empty = em & byPosition . at pos .~ Nothing
    setter em (sort -> entities) =
      let origEIDs = maybe Empty toNullable $ em ^. byPosition . at pos
          origEntitiesWithIDs =
            sortOn snd $ toList origEIDs <&> \eid -> (eid, getEIDAssume em eid)
          go alles₁@((eid, e₁) :< es₁) -- orig
             (e₂ :< es₂)               -- new
            | e₁ == e₂
              -- same, do nothing
            = let (eids, lastEID, byID') = go es₁ es₂
              in (insertSet eid eids, lastEID, byID')
            | otherwise
              -- e₂ is new, generate a new ID for it
            = let (eids, lastEID, byID') = go alles₁ es₂
                  eid' = succ lastEID
              in (insertSet eid' eids, eid', byID' & at eid' ?~ Positioned pos e₂)
          go Empty Empty = (mempty, em ^. lastID, em ^. byID)
          go orig Empty =
            let byID' = foldr deleteMap (em ^. byID) $ map fst orig
            in (mempty, em ^. lastID, byID')
          go Empty (new :< news) =
            let (eids, lastEID, byID') = go Empty news
                eid' = succ lastEID
            in (insertSet eid' eids, eid', byID' & at eid' ?~ Positioned pos new)
          go _ _ = error "unreachable"
          (eidsAtPosition, newLastID, newByID) = go origEntitiesWithIDs entities
      in em & byPosition . at pos .~ fromNullable eidsAtPosition
            & byID .~ newByID
            & lastID .~ newLastID

getEIDAssume :: EntityMap a -> EntityID -> a
getEIDAssume em eid = fromMaybe byIDInvariantError
  $ em ^? byID . ix eid . positioned

atPositionWithIDs :: Position -> EntityMap a -> Vector (EntityID, Positioned a)
atPositionWithIDs pos em =
  let eids = maybe mempty (toVector . toNullable)
             $ em ^. byPosition . at pos
  in (id &&& Positioned pos . getEIDAssume em) <$> eids

fromEIDsAndPositioned
  :: forall mono a. (MonoFoldable mono, Element mono ~ (EntityID, Positioned a))
  => mono
  -> EntityMap a
fromEIDsAndPositioned eps = newLastID $ alaf Endo foldMap insert' eps mempty
  where
    insert' (eid, pe@(Positioned pos _))
      = (byID . at eid ?~ pe)
      . (byPosition . at pos %~ \case
            Just eids -> Just $ ninsertSet eid eids
            Nothing   -> Just $ opoint eid
        )
    newLastID em = em & lastID
      .~ fromMaybe 1
         (maximumOf (ifolded . asIndex) (em ^. byID))

toEIDsAndPositioned :: EntityMap a -> [(EntityID, Positioned a)]
toEIDsAndPositioned = itoListOf $ byID . ifolded

positions :: EntityMap a -> [Position]
positions = toListOf $ byPosition . to keys . folded

lookupWithPosition :: EntityID -> EntityMap a -> Maybe (Positioned a)
lookupWithPosition eid = view $ byID . at eid

lookup :: EntityID -> EntityMap a -> Maybe a
lookup eid = fmap (view positioned) . lookupWithPosition eid

-- unlawful :(
-- positionedEntities :: IndexedTraversal EntityID (EntityMap a) (EntityMap b) (Positioned a) (Positioned b)
-- positionedEntities = byID . itraversed

neighbors :: (Ord a, Show a) => Position -> EntityMap a -> Neighbors (VectorBag a)
neighbors pos em = (\p -> view (atPosition p) em) <$> neighborPositions pos

--------------------------------------------------------------------------------
makeWrapped ''Deduplicate

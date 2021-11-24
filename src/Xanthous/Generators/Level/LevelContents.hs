{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Level.LevelContents
  ( chooseCharacterPosition
  , randomItems
  , randomCreatures
  , randomDoors
  , placeDownStaircase
  , tutorialMessage
  , entityFromRaw
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (any, toList)
--------------------------------------------------------------------------------
import           Control.Monad.Random
import           Data.Array.IArray (amap, bounds, rangeSize, (!))
import qualified Data.Array.IArray as Arr
import           Data.Foldable (any, toList)
import           Linear.V2
--------------------------------------------------------------------------------
import           Xanthous.Generators.Level.Util
import           Xanthous.Random hiding (chance)
import qualified Xanthous.Random as Random
import           Xanthous.Data
                 ( positionFromV2,  Position, _Position
                 , rotations, arrayNeighbors, Neighbors(..)
                 , neighborPositions
                 )
import           Xanthous.Data.EntityMap (EntityMap, _EntityMap)
import           Xanthous.Entities.Raws (rawsWithType, RawType, raw)
import qualified Xanthous.Entities.Item as Item
import           Xanthous.Entities.Item (Item)
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Creature (Creature)
import           Xanthous.Entities.Environment
                 (GroundMessage(..), Door(..), unlockedDoor, Staircase(..))
import           Xanthous.Messages (message_)
import           Xanthous.Util.Graphics (circle)
import           Xanthous.Entities.RawTypes
import           Xanthous.Entities.Creature.Hippocampus (initialHippocampus)
import           Xanthous.Entities.Common (inRightHand, asWieldedItem, wielded)
import           Xanthous.Game.State (SomeEntity(SomeEntity))
--------------------------------------------------------------------------------

chooseCharacterPosition :: MonadRandom m => Cells -> m Position
chooseCharacterPosition = randomPosition

randomItems :: MonadRandom m => Cells -> m (EntityMap Item)
randomItems = randomEntities (fmap Identity . Item.newWithType) (0.0004, 0.001)

placeDownStaircase :: MonadRandom m => Cells -> m (EntityMap Staircase)
placeDownStaircase cells = do
  pos <- randomPosition cells
  pure $ _EntityMap # [(pos, DownStaircase)]

randomDoors :: MonadRandom m => Cells -> m (EntityMap Door)
randomDoors cells = do
  doorRatio <- getRandomR subsetRange
  let numDoors = floor $ doorRatio * fromIntegral (length candidateCells)
      doorPositions =
        removeAdjacent . fmap positionFromV2 . take numDoors $ candidateCells
      doors = zip doorPositions $ repeat unlockedDoor
  pure $ _EntityMap # doors
  where
    removeAdjacent =
      foldr (\pos acc ->
               if pos `elem` (acc >>= toList . neighborPositions)
               then acc
               else pos : acc
            ) []
    candidateCells = filter doorable $ Arr.indices cells
    subsetRange = (0.8 :: Double, 1.0)
    doorable pos =
      not (fromMaybe True $ cells ^? ix pos)
      && any (teeish . fmap (fromMaybe True))
        (rotations $ arrayNeighbors cells pos)
    -- only generate doors at the *ends* of hallways, eg (where O is walkable,
    -- X is a wall, and D is a door):
    --
    -- O O O
    -- X D X
    --   O
    teeish (fmap not -> (Neighbors tl t tr l r _ b _ )) =
      and [tl, t, tr, b] && (and . fmap not) [l, r]

randomCreatures
  :: MonadRandom m
  => Word -- ^ Level number, starting at 0
  -> Cells
  -> m (EntityMap Creature)
randomCreatures levelNumber
  = randomEntities maybeNewCreature (0.0007, 0.002)
  where
    maybeNewCreature cType
      | maybe True (canGenerate levelNumber) $ cType ^. generateParams
      = Just <$> newCreatureWithType cType
      | otherwise
      = pure Nothing

newCreatureWithType :: MonadRandom m => CreatureType -> m Creature
newCreatureWithType _creatureType = do
  let _hitpoints = _creatureType ^. maxHitpoints
      _hippocampus = initialHippocampus

  equipped <- fmap join
            . traverse genEquipped
            $ _creatureType
            ^.. generateParams . _Just . equippedItem . _Just
  let _inventory = maybe id (\ei -> wielded .~ inRightHand ei) (headMay equipped) mempty
  pure Creature.Creature {..}
  where
    genEquipped cei = do
      doGen <- Random.chance $ cei ^. chance
      let entName = cei ^. entityName
          itemType =
            fromMaybe (error $ "raw \"" <> unpack entName <> "\" not of type Item")
            . preview _Item
            . fromMaybe (error $ "Could not find raw: " <> unpack entName)
            $ raw entName
      item <- Item.newWithType itemType
      if doGen
        then pure [fromMaybe (error $ "raw \"" <> unpack entName <> "\" not wieldable")
                  $ preview asWieldedItem item]
        else pure []


tutorialMessage :: MonadRandom m
  => Cells
  -> Position -- ^ CharacterPosition
  -> m (EntityMap GroundMessage)
tutorialMessage cells characterPosition = do
  let distance = 2
  pos <- fmap (fromMaybe (error "No valid positions for tutorial message?"))
        . choose . ChooseElement
        $ accessiblePositionsWithin distance cells characterPosition
  msg <- message_ ["tutorial", "message1"]
  pure $ _EntityMap # [(pos, GroundMessage msg)]
  where
    accessiblePositionsWithin :: Int -> Cells -> Position -> [Position]
    accessiblePositionsWithin dist valid pos =
      review _Position
      <$> filter
            (\pt -> not $ valid ! (fromIntegral <$> pt))
            (circle (pos ^. _Position) dist)

randomEntities
  :: forall entity raw m t. (MonadRandom m, RawType raw, Functor t, Foldable t)
  => (raw -> m (t entity))
  -> (Float, Float)
  -> Cells
  -> m (EntityMap entity)
randomEntities newWithType sizeRange cells =
  case fromNullable $ rawsWithType @raw of
    Nothing -> pure mempty
    Just raws -> do
      let len = rangeSize $ bounds cells
      (numEntities :: Int) <-
        floor . (* fromIntegral len) <$> getRandomR sizeRange
      entities <- for [0..numEntities] $ const $ do
        pos <- randomPosition cells
        r <- choose raws
        entities <- newWithType r
        pure $ (pos, ) <$> entities
      pure $ _EntityMap # (entities >>= toList)

randomPosition :: MonadRandom m => Cells -> m Position
randomPosition = fmap positionFromV2 . choose . impureNonNull . cellCandidates

-- cellCandidates :: Cells -> Cells
cellCandidates :: Cells -> Set (V2 Word)
cellCandidates
  -- find the largest contiguous region of cells in the cave.
  = maximumBy (compare `on` length)
  . fromMaybe (error "No regions generated! this should never happen.")
  . fromNullable
  . regions
  -- cells ends up with true = wall, we want true = can put an item here
  . amap not

entityFromRaw :: MonadRandom m => EntityRaw -> m SomeEntity
entityFromRaw (Creature ct) = SomeEntity <$> newCreatureWithType ct
entityFromRaw (Item it) = SomeEntity <$> Item.newWithType it

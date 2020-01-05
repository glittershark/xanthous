--------------------------------------------------------------------------------
module Xanthous.Generators.LevelContents
  ( chooseCharacterPosition
  , randomItems
  , randomCreatures
  , randomDoors
  , placeDownStaircase
  , tutorialMessage
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Control.Monad.Random
import           Data.Array.IArray (amap, bounds, rangeSize, (!))
import qualified Data.Array.IArray as Arr
--------------------------------------------------------------------------------
import           Xanthous.Generators.Util
import           Xanthous.Random
import           Xanthous.Data (Position, _Position, positionFromPair)
import           Xanthous.Data.EntityMap (EntityMap, _EntityMap)
import           Xanthous.Entities.Raws (rawsWithType, RawType)
import qualified Xanthous.Entities.Item as Item
import           Xanthous.Entities.Item (Item)
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Creature (Creature)
import           Xanthous.Entities.Environment
                 (GroundMessage(..), Door(..), unlockedDoor, Staircase(..))
import           Xanthous.Messages (message_)
import           Xanthous.Util.Graphics (circle)
--------------------------------------------------------------------------------

chooseCharacterPosition :: MonadRandom m => Cells -> m Position
chooseCharacterPosition = randomPosition

randomItems :: MonadRandom m => Cells -> m (EntityMap Item)
randomItems = randomEntities Item.newWithType (0.0004, 0.001)

placeDownStaircase :: MonadRandom m => Cells -> m (EntityMap Staircase)
placeDownStaircase cells = do
  pos <- randomPosition cells
  pure $ _EntityMap # [(pos, DownStaircase)]

randomDoors :: MonadRandom m => Cells -> m (EntityMap Door)
randomDoors cells = do
  doorRatio <- getRandomR subsetRange
  let numDoors = floor $ doorRatio * fromIntegral (length candidateCells)
      doorPositions = positionFromPair <$> take numDoors candidateCells
      doors = zip doorPositions $ repeat unlockedDoor
  pure $ _EntityMap # doors
  where
    candidateCells = filter doorable $ Arr.indices cells
    subsetRange = (0.8 :: Double, 1.0)
    doorable (x, y) =
      not (fromMaybe True $ cells ^? ix (x, y))
      &&
      ( fromMaybe True $ cells ^? ix (x - 1, y) -- left
      , fromMaybe True $ cells ^? ix (x, y - 1) -- top
      , fromMaybe True $ cells ^? ix (x + 1, y) -- right
      , fromMaybe True $ cells ^? ix (x, y + 1) -- bottom
      ) `elem` [ (True, False, True, False)
          , (False, True, False, True)
          ]

randomCreatures :: MonadRandom m => Cells -> m (EntityMap Creature)
randomCreatures = randomEntities Creature.newWithType (0.0007, 0.003)

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
      <$> filter (\(px, py) -> not $ valid ! (fromIntegral px, fromIntegral py))
          (circle (pos ^. _Position) dist)

randomEntities
  :: forall entity raw m. (MonadRandom m, RawType raw)
  => (raw -> entity)
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
        raw <- choose raws
        let entity = newWithType raw
        pure (pos, entity)
      pure $ _EntityMap # entities

randomPosition :: MonadRandom m => Cells -> m Position
randomPosition = fmap positionFromPair . choose . impureNonNull . cellCandidates

-- cellCandidates :: Cells -> Cells
cellCandidates :: Cells -> Set (Word, Word)
cellCandidates
  -- find the largest contiguous region of cells in the cave.
  = maximumBy (compare `on` length)
  . fromMaybe (error "No regions generated! this should never happen.")
  . fromNullable
  . regions
  -- cells ends up with true = wall, we want true = can put an item here
  . amap not

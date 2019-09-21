--------------------------------------------------------------------------------
module Xanthous.Generators.LevelContents
  ( chooseCharacterPosition
  , randomItems
  , randomCreatures
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Control.Monad.Random
import           Data.Array.IArray (amap, bounds, rangeSize)
--------------------------------------------------------------------------------
import           Xanthous.Generators.Util
import           Xanthous.Random
import           Xanthous.Data (Position, positionFromPair)
import           Xanthous.Data.EntityMap (EntityMap, _EntityMap)
import           Xanthous.Entities.Raws (rawsWithType, RawType)
import qualified Xanthous.Entities.Item as Item
import           Xanthous.Entities.Item (Item)
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Creature (Creature)
--------------------------------------------------------------------------------

chooseCharacterPosition :: MonadRandom m => Cells -> m Position
chooseCharacterPosition = randomPosition

randomItems :: MonadRandom m => Cells -> m (EntityMap Item)
randomItems = randomEntities Item.newWithType (0.0004, 0.001)

randomCreatures :: MonadRandom m => Cells -> m (EntityMap Creature)
randomCreatures = randomEntities Creature.newWithType (0.0007, 0.003)

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
      (numEntities :: Int) <- floor . (* fromIntegral len) <$> getRandomR sizeRange
      entities <- for [0..numEntities] $ const $ do
        pos <- randomPosition cells
        raw <- choose raws
        let entity = newWithType raw
        pure (pos, entity)
      pure $ _EntityMap # entities

randomPosition :: MonadRandom m => Cells -> m Position
randomPosition cells = fmap positionFromPair . choose $ impureNonNull candidates
  where
    -- cells ends up with true = wall, we want true = can put an item here
    placeableCells = amap not cells

    -- find the largest contiguous region of cells in the cave.
    candidates
      = maximumBy (compare `on` length)
      $ fromMaybe (error "No regions generated! this should never happen.")
      $ fromNullable
      $ regions placeableCells

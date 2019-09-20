--------------------------------------------------------------------------------
module Xanthous.Generators.LevelContents
  ( chooseCharacterPosition
  , randomItems
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
import           Xanthous.Entities.Item (Item(..))
import           Xanthous.Entities.Raws
import           Xanthous.Entities.RawTypes
import qualified Xanthous.Entities.Item as Item
--------------------------------------------------------------------------------

chooseCharacterPosition :: MonadRandom m => Cells -> m Position
chooseCharacterPosition = randomPosition

randomItems :: MonadRandom m => Cells -> m (EntityMap Item)
randomItems cells = do
  let len = rangeSize $ bounds cells
  (numItems :: Int) <- floor . (* fromIntegral len)
                     <$> getRandomR @_ @Float (0.0004, 0.001)
  items <- for [0..numItems] $ const $ do
    pos <- randomPosition cells
    itemType <- fmap (fromMaybe (error "no item raws!"))
               . choose . ChooseElement
               $ rawsWithType @ItemType
    let item = Item.newWithType itemType
    pure (pos, item)
  pure $ _EntityMap # items

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

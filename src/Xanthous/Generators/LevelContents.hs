--------------------------------------------------------------------------------
module Xanthous.Generators.LevelContents
  ( chooseCharacterPosition
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Control.Monad.Random
import Data.Array.IArray (amap)
--------------------------------------------------------------------------------
import Xanthous.Generators.Util
import Xanthous.Random
--------------------------------------------------------------------------------

chooseCharacterPosition :: MonadRandom m => Cells -> m (Word, Word)
chooseCharacterPosition cells = choose $ impureNonNull candidates
  where
    -- cells ends up with true = wall, we want true = can put a character here
    placeableCells = amap not cells

    -- find the largest contiguous region of cells in the cave.
    candidates
      = maximumBy (compare `on` length)
      $ fromMaybe (error "No regions generated! this should never happen.")
      $ fromNullable
      $ regions placeableCells

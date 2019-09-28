{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.AI.Gormlak () where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (lines)
--------------------------------------------------------------------------------
import           Data.Coerce
import           Control.Monad.State
--------------------------------------------------------------------------------
import           Xanthous.Data (Positioned(..))
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Creature (Creature)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Entities (Entity(..), Brain(..), brainVia)
import           Xanthous.Game.State (entities, GameState)
import           Xanthous.Data.EntityMap.Graphics (linesOfSight)
--------------------------------------------------------------------------------

stepGormlak :: MonadState GameState m => Positioned Creature -> m (Positioned Creature)
stepGormlak (Positioned pos creature) = do
  lines <- uses entities $ linesOfSight pos (Creature.visionRadius creature)
  let newPos = fromMaybe pos
               $ fmap fst
               . headMay <=< tailMay <=< headMay
               . sortOn (Down . length)
               $ lines
  pure $ Positioned newPos creature

newtype GormlakBrain = GormlakBrain Creature

instance Brain GormlakBrain where
  step = fmap coerce . stepGormlak . coerce
--------------------------------------------------------------------------------

instance Brain Creature where step = brainVia GormlakBrain

instance Entity Creature where
  blocksVision _ = False
  description = view $ Creature.creatureType . Raw.description

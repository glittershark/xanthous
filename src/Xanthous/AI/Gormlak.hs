{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.AI.Gormlak () where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (lines)
--------------------------------------------------------------------------------
import           Data.Coerce
import           Control.Monad.State
import           Control.Monad.Random
--------------------------------------------------------------------------------
import           Xanthous.Data (Positioned(..), positioned)
import           Xanthous.Data.EntityMap
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Creature (Creature)
import           Xanthous.Entities.Character (Character)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Entities (Entity(..), Brain(..), brainVia)
import           Xanthous.Game.State (entities, GameState, entityIs)
import           Xanthous.Game.Lenses (Collision(..), collisionAt)
import           Xanthous.Data.EntityMap.Graphics (linesOfSight)
import           Xanthous.Random
--------------------------------------------------------------------------------

stepGormlak
  :: (MonadState GameState m, MonadRandom m)
  => Positioned Creature
  -> m (Positioned Creature)
stepGormlak pe@(Positioned pos creature) = do
  lines <- uses entities $ linesOfSight pos (Creature.visionRadius creature)
  line <- choose $ weightedBy length lines
  -- traceShowM ("current position", pos)
  -- traceShowM ("lines", (headMay <=< tailMay) <$> lines)
  let newPos = fromMaybe pos
               $ fmap fst
               . headMay
               =<< tailMay
               =<< line
  collisionAt newPos >>= \case
    Nothing -> pure $ Positioned newPos creature
    Just Stop -> pure pe
    Just Combat -> do
      ents <- use $ entities . atPosition newPos
      if | any (entityIs @Creature) ents -> pure pe
         | any (entityIs @Character) ents -> undefined
         | otherwise -> pure pe

newtype GormlakBrain = GormlakBrain Creature

instance Brain GormlakBrain where
  step = fmap coerce . stepGormlak . coerce
--------------------------------------------------------------------------------

instance Brain Creature where step = brainVia GormlakBrain

instance Entity Creature where
  blocksVision _ = False
  description = view $ Creature.creatureType . Raw.description

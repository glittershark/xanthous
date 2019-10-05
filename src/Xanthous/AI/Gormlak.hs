{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.AI.Gormlak () where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (lines)
--------------------------------------------------------------------------------
import           Data.Coerce
import           Control.Monad.State
import           Control.Monad.Random
import           Data.Aeson (object)
import qualified Data.Aeson as A
--------------------------------------------------------------------------------
import           Xanthous.Data (Positioned(..), diffPositions, stepTowards, isUnit)
import           Xanthous.Data.EntityMap
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Creature (Creature)
import           Xanthous.Entities.Character (Character, characterHitpoints)
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Entities (Entity(..), Brain(..), brainVia)
import           Xanthous.Game.State (entities, GameState, entityIs)
import           Xanthous.Game.Lenses
                 ( Collision(..), collisionAt, character, characterPosition )
import           Xanthous.Data.EntityMap.Graphics (linesOfSight, canSee)
import           Xanthous.Random
import           Xanthous.Monad (say)
--------------------------------------------------------------------------------

stepGormlak
  :: (MonadState GameState m, MonadRandom m)
  => Positioned Creature
  -> m (Positioned Creature)
stepGormlak pe@(Positioned pos creature) = do
  newPos <- do
    canSeeCharacter <- uses entities $ canSee (entityIs @Character) pos vision
    if canSeeCharacter
      then do
        charPos <- use characterPosition
        if isUnit (pos `diffPositions` charPos)
          then attackCharacter $> pos
          else pure $ pos `stepTowards` charPos
    else do
      lines <- uses entities $ linesOfSight pos (Creature.visionRadius creature)
      line <- choose $ weightedBy length lines
      pure $ fromMaybe pos $ fmap fst . headMay =<< tailMay =<< line
  collisionAt newPos >>= \case
    Nothing -> pure $ Positioned newPos creature
    Just Stop -> pure pe
    Just Combat -> do
      ents <- use $ entities . atPosition newPos
      when (any (entityIs @Character) ents) attackCharacter
      pure pe

  where
    vision = Creature.visionRadius creature
    attackCharacter = do
      say ["combat", "creatureAttack"] $ object [ "creature" A..= creature ]
      character . characterHitpoints -= 1

newtype GormlakBrain = GormlakBrain Creature

instance Brain GormlakBrain where
  step = fmap coerce . stepGormlak . coerce

--------------------------------------------------------------------------------

instance Brain Creature where step = brainVia GormlakBrain

instance Entity Creature where
  blocksVision _ = False
  description = view $ Creature.creatureType . Raw.description

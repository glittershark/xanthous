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
import           Xanthous.Data
                 ( Positioned(..), positioned, position
                 , diffPositions, stepTowards, isUnit
                 , Ticks, (|*|), invertedRate
                 )
import           Xanthous.Data.EntityMap
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Creature
                 ( Creature, hippocampus, creatureType
                 , destination, destinationProgress, destinationPosition
                 )
import           Xanthous.Entities.Character (Character)
import qualified Xanthous.Entities.Character as Character
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Entities (Entity(..), Brain(..), brainVia)
import           Xanthous.Game.State (entities, GameState, entityIs)
import           Xanthous.Game.Lenses
                 ( Collision(..), entityCollision, collisionAt
                 , character, characterPosition
                 )
import           Xanthous.Data.EntityMap.Graphics (linesOfSight, canSee)
import           Xanthous.Random
import           Xanthous.Monad (say)
--------------------------------------------------------------------------------

stepGormlak
  :: (MonadState GameState m, MonadRandom m)
  => Ticks
  -> Positioned Creature
  -> m (Positioned Creature)
stepGormlak ticks pe@(Positioned pos creature) = do
  dest <- maybe (selectDestination pos creature) pure
         $ creature ^. hippocampus . destination
  let progress' =
        dest ^. destinationProgress
        + creature ^. creatureType . Raw.speed . invertedRate |*| ticks
  if progress' < 1
    then pure
         $ pe
         & positioned . hippocampus . destination
         ?~ (dest & destinationProgress .~ progress')
    else do
      let newPos = dest ^. destinationPosition
          remainingSpeed = progress' - 1
      newDest <- selectDestination newPos creature
                <&> destinationProgress +~ remainingSpeed
      let pe' = pe & positioned . hippocampus . destination ?~ newDest
      collisionAt newPos >>= \case
        Nothing -> pure $ pe' & position .~ newPos
        Just Stop -> pure pe'
        Just Combat -> do
          ents <- use $ entities . atPosition newPos
          when (any (entityIs @Character) ents) attackCharacter
          pure pe'
  where
    selectDestination pos' creature' = Creature.destinationFromPos <$> do
      canSeeCharacter <- uses entities $ canSee (entityIs @Character) pos' vision
      if canSeeCharacter
        then do
          charPos <- use characterPosition
          if isUnit (pos' `diffPositions` charPos)
            then attackCharacter $> pos'
            else pure $ pos' `stepTowards` charPos
      else do
        lines <- map (takeWhile (isNothing . entityCollision . map snd . snd)
                    -- the first item on these lines is always the creature itself
                    . fromMaybe mempty . tailMay)
                . linesOfSight pos' (Creature.visionRadius creature')
                <$> use entities
        line <- choose $ weightedBy length lines
        pure $ fromMaybe pos' $ fmap fst . headMay =<< line

    vision = Creature.visionRadius creature
    attackCharacter = do
      say ["combat", "creatureAttack"] $ object [ "creature" A..= creature ]
      character %= Character.damage 1

newtype GormlakBrain = GormlakBrain Creature

instance Brain GormlakBrain where
  step ticks = fmap coerce . stepGormlak ticks . coerce

--------------------------------------------------------------------------------

instance Brain Creature where step = brainVia GormlakBrain

instance Entity Creature where
  blocksVision _ = False
  description = view $ Creature.creatureType . Raw.description

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
module Xanthous.AI.Gormlak
  ( HasVisionRadius(..)
  , GormlakBrain(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (lines)
--------------------------------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Random
import           Data.Aeson (object)
import qualified Data.Aeson as A
import           Data.Generics.Product.Fields
--------------------------------------------------------------------------------
import           Xanthous.Data
                 ( Positioned(..), positioned, position, _Position
                 , diffPositions, stepTowards, isUnit
                 , Ticks, (|*|), invertedRate
                 )
import           Xanthous.Data.EntityMap
import           Xanthous.Entities.Creature.Hippocampus
import           Xanthous.Entities.Character (Character)
import qualified Xanthous.Entities.Character as Character
import qualified Xanthous.Entities.RawTypes as Raw
import           Xanthous.Entities.RawTypes (CreatureType, HasLanguage (language), getLanguage)
import           Xanthous.Game.State
import           Xanthous.Game.Lenses
                 ( entitiesCollision, collisionAt
                 , character, characterPosition, positionIsCharacterVisible
                 , hearingRadius
                 )
import           Xanthous.Data.EntityMap.Graphics (linesOfSight, canSee)
import           Xanthous.Random
import           Xanthous.Monad (say)
import           Xanthous.Generators.Speech (word)
import qualified Linear.Metric as Metric
--------------------------------------------------------------------------------

--  TODO move the following two classes to a more central location

class HasVisionRadius a where visionRadius :: a -> Word

type IsCreature entity =
  ( HasVisionRadius entity
  , HasField "_hippocampus" entity entity Hippocampus Hippocampus
  , HasField "_creatureType" entity entity CreatureType CreatureType
  , A.ToJSON entity
  )

--------------------------------------------------------------------------------

stepGormlak
  :: forall entity m.
    ( MonadState GameState m, MonadRandom m
    , IsCreature entity
    )
  => Ticks
  -> Positioned entity
  -> m (Positioned entity)
stepGormlak ticks pe@(Positioned pos creature) = do
  canSeeCharacter <- uses entities $ canSee (entityIs @Character) pos vision

  let selectDestination pos' creature' = destinationFromPos <$> do
        if canSeeCharacter
          then do
            charPos <- use characterPosition
            if isUnit (pos' `diffPositions` charPos)
              then attackCharacter $> pos'
              else pure $ pos' `stepTowards` charPos
        else do
          lines <- map (takeWhile (isNothing . entitiesCollision . map snd . snd)
                      -- the first item on these lines is always the creature itself
                      . fromMaybe mempty . tailMay)
                  . linesOfSight pos' (visionRadius creature')
                  <$> use entities
          line <- choose $ weightedBy length lines
          pure $ fromMaybe pos' $ fmap fst . headMay =<< line

  pe' <- if canSeeCharacter && not (creature ^. creatureGreeted)
        then yellAtCharacter $> (pe & positioned . creatureGreeted .~ True)
        else pure pe

  dest <- maybe (selectDestination pos creature) pure
         $ creature ^. field @"_hippocampus" . destination
  let progress' =
        dest ^. destinationProgress
        + creature ^. field @"_creatureType" . Raw.speed . invertedRate |*| ticks
  if progress' < 1
    then pure
         $ pe'
         & positioned . field @"_hippocampus" . destination
         ?~ (dest & destinationProgress .~ progress')
    else do
      let newPos = dest ^. destinationPosition
          remainingSpeed = progress' - 1
      newDest <- selectDestination newPos creature
                <&> destinationProgress +~ remainingSpeed
      let pe'' = pe' & positioned . field @"_hippocampus" . destination ?~ newDest
      collisionAt newPos >>= \case
        Nothing -> pure $ pe'' & position .~ newPos
        Just Stop -> pure pe''
        Just Combat -> do
          ents <- use $ entities . atPosition newPos
          when (any (entityIs @Character) ents) attackCharacter
          pure pe'
  where
    vision = visionRadius creature
    attackCharacter = do
      say ["combat", "creatureAttack"] $ object [ "creature" A..= creature ]
      character %= Character.damage 1

    yellAtCharacter = for_ (creature ^. field @"_creatureType" . language)
      $ \lang -> do
          utterance <- fmap (<> "!") . word $ getLanguage lang
          creatureSaysText pe utterance

    creatureGreeted :: Lens' entity Bool
    creatureGreeted = field @"_hippocampus" . greetedCharacter


-- | A creature sends some text
--
-- If that creature is visible to the character, its description will be
-- included, otherwise if it's within earshot the character will just hear the
-- sound
creatureSaysText
  :: (MonadState GameState m, MonadRandom m, IsCreature entity)
  => Positioned entity
  -> Text
  -> m ()
creatureSaysText ent txt = do
  let entPos = ent ^. position . _Position . to (fmap fromIntegral)
  charPos <- use $ characterPosition . _Position . to (fmap fromIntegral)
  let dist :: Int
      dist = round $ Metric.distance @_ @Double entPos charPos
      audible = dist <= fromIntegral hearingRadius
  when audible $ do
    visible <- positionIsCharacterVisible $ ent ^. position
    let path = ["entities", "say", "creature"]
               <> [if visible then "visible" else "invisible"]
        params = object [ "creature" A..= (ent ^. positioned)
                        , "message" A..= txt
                        ]
    say path params

newtype GormlakBrain entity = GormlakBrain { _unGormlakBrain :: entity }

instance (IsCreature entity) => Brain (GormlakBrain entity) where
  step ticks
    = fmap (fmap GormlakBrain)
    . stepGormlak ticks
    . fmap _unGormlakBrain
  entityCanMove = const True

--------------------------------------------------------------------------------

-- instance Brain Creature where
--   step = brainVia GormlakBrain
--   entityCanMove = const True

-- instance Entity Creature where
--   blocksVision _ = False
--   description = view $ Creature.creatureType . Raw.description
--   entityChar = view $ Creature.creatureType . char

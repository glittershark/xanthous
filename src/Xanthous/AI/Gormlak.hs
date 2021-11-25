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
import           Xanthous.Entities.RawTypes
                 ( CreatureType, HasLanguage(language), getLanguage
                 , HasAttacks (attacks), creatureAttackMessage
                 )
import           Xanthous.Entities.Common
                 ( wielded, Inventory, wieldedItems, WieldedItem (WieldedItem) )
import           Xanthous.Game.State
import           Xanthous.Game.Lenses
                 ( entitiesCollision, collisionAt
                 , character, characterPosition, positionIsCharacterVisible
                 , hearingRadius
                 )
import           Xanthous.Data.EntityMap.Graphics (linesOfSight, canSee)
import           Xanthous.Random
import           Xanthous.Monad (say, message)
import           Xanthous.Generators.Speech (word)
import qualified Linear.Metric as Metric
import qualified Xanthous.Messages as Messages
--------------------------------------------------------------------------------

--  TODO move the following two classes to a more central location

class HasVisionRadius a where visionRadius :: a -> Word

type IsCreature entity =
  ( HasVisionRadius entity
  , HasField "_hippocampus" entity entity Hippocampus Hippocampus
  , HasField "_creatureType" entity entity CreatureType CreatureType
  , HasField "_inventory" entity entity Inventory Inventory
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
         . mfilter (\(Destination p _) -> p /= pos)
         $ creature ^. hippocampus . destination
  let progress' =
        dest ^. destinationProgress
        + creature ^. creatureType . Raw.speed . invertedRate |*| ticks
  if progress' < 1
    then pure
         $ pe'
         & positioned . hippocampus . destination
         ?~ (dest & destinationProgress .~ progress')
    else do
      let newPos = dest ^. destinationPosition
          remainingSpeed = progress' - 1
      newDest <- selectDestination newPos creature
                <&> destinationProgress +~ remainingSpeed
      let pe'' = pe' & positioned . hippocampus . destination ?~ newDest
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
      dmg <- case creature ^? inventory . wielded . wieldedItems of
        Just (WieldedItem item wi) -> do
          let msg = fromMaybe
                    (Messages.lookup ["combat", "creatureAttack", "genericWeapon"])
                    $ wi ^. creatureAttackMessage
          message msg $ object [ "creature" A..= creature
                               , "item" A..= item
                               ]
          pure $ wi ^. Raw.damage
        Nothing -> do
          attack <- choose $ creature ^. creatureType . attacks
          attackDescription <- Messages.render (attack ^. Raw.description)
                              $ object []
          say ["combat", "creatureAttack", "natural"]
              $ object [ "creature" A..= creature
                       , "attackDescription" A..= attackDescription
                       ]
          pure $ attack ^. Raw.damage

      character %= Character.damage dmg

    yellAtCharacter = for_ (creature ^. creatureType . language)
      $ \lang -> do
          utterance <- fmap (<> "!") . word $ getLanguage lang
          creatureSaysText pe utterance

    creatureGreeted :: Lens' entity Bool
    creatureGreeted = hippocampus . greetedCharacter


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

hippocampus :: HasField "_hippocampus" s t a b => Lens s t a b
hippocampus = field @"_hippocampus"

creatureType :: HasField "_creatureType" s t a b => Lens s t a b
creatureType = field @"_creatureType"

inventory :: HasField "_inventory" s t a b => Lens s t a b
inventory = field @"_inventory"

--------------------------------------------------------------------------------

-- instance Brain Creature where
--   step = brainVia GormlakBrain
--   entityCanMove = const True

-- instance Entity Creature where
--   blocksVision _ = False
--   description = view $ Creature.creatureType . Raw.description
--   entityChar = view $ Creature.creatureType . char

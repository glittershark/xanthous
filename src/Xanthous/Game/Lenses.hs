{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Lenses
  ( positionedCharacter
  , character
  , characterPosition
  , updateCharacterVision
  , characterVisiblePositions
  , characterVisibleEntities
  , getInitialState
  , initialStateFromSeed
  , entitiesAtCharacter
  , revealedEntitiesAtPosition

    -- * Collisions
  , Collision(..)
  , entitiesCollision
  , collisionAt
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           System.Random
import           Control.Monad.State
import           Control.Monad.Random (getRandom)
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Data
import           Xanthous.Data.Levels
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Data.EntityMap.Graphics
                 (visiblePositions, visibleEntities)
import           Xanthous.Data.VectorBag
import           Xanthous.Entities.Character (Character, mkCharacter)
import           {-# SOURCE #-} Xanthous.Entities.Entities ()
--------------------------------------------------------------------------------

getInitialState :: IO GameState
getInitialState = initialStateFromSeed <$> getRandom

initialStateFromSeed :: Int -> GameState
initialStateFromSeed seed =
  let _randomGen = mkStdGen seed
      chr = mkCharacter
      _upStaircasePosition = Position 0 0
      (_characterEntityID, _levelEntities)
        = EntityMap.insertAtReturningID
          _upStaircasePosition
          (SomeEntity chr)
          mempty
      _levelRevealedPositions = mempty
      level = GameLevel {..}
      _levels = oneLevel level
      _messageHistory = mempty
      _promptState = NoPrompt
      _activePanel = Nothing
      _debugState = DebugState
        { _allRevealed = False
        }
      _autocommand = NoAutocommand
  in GameState {..}


positionedCharacter :: Lens' GameState (Positioned Character)
positionedCharacter = lens getPositionedCharacter setPositionedCharacter
  where
    setPositionedCharacter :: GameState -> Positioned Character -> GameState
    setPositionedCharacter game chr
      = game
      &  entities . at (game ^. characterEntityID)
      ?~ fmap SomeEntity chr

    getPositionedCharacter :: GameState -> Positioned Character
    getPositionedCharacter game
      = over positioned
        ( fromMaybe (error "Invariant error: Character was not a character!")
        . downcastEntity
        )
      . fromMaybe (error "Invariant error: Character not found!")
      $ EntityMap.lookupWithPosition
        (game ^. characterEntityID)
        (game ^. entities)


character :: Lens' GameState Character
character = positionedCharacter . positioned

characterPosition :: Lens' GameState Position
characterPosition = positionedCharacter . position

visionRadius :: Word
visionRadius = 12 -- TODO make this dynamic

-- | Update the revealed entities at the character's position based on their
-- vision
updateCharacterVision :: GameState -> GameState
updateCharacterVision game
  = game & revealedPositions <>~ characterVisiblePositions game

characterVisiblePositions :: GameState -> Set Position
characterVisiblePositions game =
  let charPos = game ^. characterPosition
  in visiblePositions charPos visionRadius $ game ^. entities

characterVisibleEntities :: GameState -> EntityMap.EntityMap SomeEntity
characterVisibleEntities game =
  let charPos = game ^. characterPosition
  in visibleEntities charPos visionRadius $ game ^. entities

entitiesCollision
  :: ( Functor f
    , forall xx. MonoFoldable (f xx)
    , Element (f SomeEntity) ~ SomeEntity
    , Element (f (Maybe Collision)) ~ Maybe Collision
    , Show (f (Maybe Collision))
    , Show (f SomeEntity)
    )
  => f SomeEntity
  -> Maybe Collision
entitiesCollision = join . maximumMay . fmap entityCollision

collisionAt :: MonadState GameState m => Position -> m (Maybe Collision)
collisionAt p = uses (entities . EntityMap.atPosition p) entitiesCollision

entitiesAtCharacter :: Lens' GameState (VectorBag SomeEntity)
entitiesAtCharacter = lens getter setter
  where
    getter gs = gs ^. entities . EntityMap.atPosition (gs ^. characterPosition)
    setter gs ents = gs
      & entities . EntityMap.atPosition (gs ^. characterPosition) .~ ents

-- | Returns all entities at the given position that are revealed to the
-- character.
--
-- Concretely, this is either entities that are *currently* visible to the
-- character, or entities, that are immobile and that the character has seen
-- before
revealedEntitiesAtPosition :: Position -> GameState -> (VectorBag SomeEntity)
revealedEntitiesAtPosition p gs
  | p `member` characterVisiblePositions gs
  = entitiesAtPosition
  | p `member` (gs ^. revealedPositions)
  = immobileEntitiesAtPosition
  | otherwise
  = mempty
  where
    entitiesAtPosition = gs ^. entities . EntityMap.atPosition p
    immobileEntitiesAtPosition = filter (not . entityCanMove) entitiesAtPosition

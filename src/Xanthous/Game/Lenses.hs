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
  , getInitialState
  , initialStateFromSeed

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
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Data.EntityMap.Graphics (visiblePositions)
import           Xanthous.Entities.Character (Character, mkCharacter)
import           {-# SOURCE #-} Xanthous.Entities.Entities ()
--------------------------------------------------------------------------------

getInitialState :: IO GameState
getInitialState = initialStateFromSeed <$> getRandom

initialStateFromSeed :: Int -> GameState
initialStateFromSeed seed =
  let _randomGen = mkStdGen seed
      chr = mkCharacter
      (_characterEntityID, _entities)
        = EntityMap.insertAtReturningID
          (Position 0 0)
          (SomeEntity chr)
          mempty
      _messageHistory = mempty
      _revealedPositions = mempty
      _promptState = NoPrompt
      _activePanel = Nothing
      _debugState = DebugState
        { _allRevealed = False
        }
      _sentWelcome = False
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

entitiesCollision
  :: ( Functor f
    , forall xx. MonoFoldable (f xx)
    , forall xx. Element (f xx) ~ xx
    , Element (f (Maybe Collision)) ~ Maybe Collision
    , Show (f (Maybe Collision))
    , Show (f SomeEntity)
    )
  => f SomeEntity
  -> Maybe Collision
entitiesCollision = join . maximumMay . fmap entityCollision

collisionAt :: MonadState GameState m => Position -> m (Maybe Collision)
collisionAt pos = uses (entities . EntityMap.atPosition pos) entitiesCollision

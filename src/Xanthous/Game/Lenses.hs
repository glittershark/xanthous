{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Lenses
  ( positionedCharacter
  , character
  , characterPosition
  , updateCharacterVision
  , getInitialState
  , initialStateFromSeed

    -- * Collisions
  , Collision(..)
  , entityCollision
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
import           Xanthous.Entities.Environment (Door, open)
import           Xanthous.Entities.Item (Item)
import           Xanthous.Entities.Creature (Creature)
import           Xanthous.Entities.Entities ()
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
      _debugState = DebugState
        { _allRevealed = False
        }
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

-- | Update the revealed entities at the character's position based on their vision
updateCharacterVision :: GameState -> GameState
updateCharacterVision game =
  let charPos = game ^. characterPosition
      visible = visiblePositions charPos visionRadius $ game ^. entities
  in game & revealedPositions <>~ visible

data Collision
  = Stop
  | Combat
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

entityCollision
  :: ( MonoFoldable (f SomeEntity)
    , Foldable f
    , Element (f SomeEntity) ~ SomeEntity
    , AsEmpty (f SomeEntity)
    )
  => f SomeEntity
  -> Maybe Collision
entityCollision Empty = Nothing
entityCollision ents
  | any (entityIs @Creature) ents = pure Combat
  | all (entityIs @Item) ents = Nothing
  | doors@(_ : _) <- ents ^.. folded . _SomeEntity @Door
  , all (view open) doors = Nothing
  | otherwise = pure Stop

collisionAt :: MonadState GameState m => Position -> m (Maybe Collision)
collisionAt pos = uses (entities . EntityMap.atPosition pos) entityCollision

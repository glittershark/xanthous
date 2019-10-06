{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Lenses
  ( positionedCharacter
  , character
  , characterPosition
  , updateCharacterVision
  , getInitialState

    -- * Collisions
  , Collision(..)
  , collisionAt
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           System.Random
import           Control.Monad.State
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Data
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Data.EntityMap.Graphics (visiblePositions)
import           Xanthous.Entities.Character (Character, mkCharacter)
import           Xanthous.Entities.Environment (Door, open)
import           Xanthous.Entities.Item (Item)
import           Xanthous.Entities.Creature (Creature)
--------------------------------------------------------------------------------

getInitialState :: IO GameState
getInitialState = do
  _randomGen <- getStdGen
  let char = mkCharacter
      (_characterEntityID, _entities)
        = EntityMap.insertAtReturningID
          (Position 0 0)
          (SomeEntity char)
          mempty
      _messageHistory = mempty
      _revealedPositions = mempty
      _promptState = NoPrompt
      _debugState = DebugState
        { _allRevealed = False
        }
  pure GameState {..}


positionedCharacter :: Lens' GameState (Positioned Character)
positionedCharacter = lens getPositionedCharacter setPositionedCharacter
  where
    setPositionedCharacter :: GameState -> Positioned Character -> GameState
    setPositionedCharacter game char
      = game
      &  entities . at (game ^. characterEntityID)
      ?~ fmap SomeEntity char

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

collisionAt :: MonadState GameState m => Position -> m (Maybe Collision)
collisionAt pos = do
  ents <- use $ entities . EntityMap.atPosition pos
  pure $
    if | null ents -> Nothing
       | any (entityIs @Creature) ents -> pure Combat
       | all (entityIs @Item) ents -> Nothing
       | doors@(_ : _) <- ents ^.. folded . _SomeEntity @Door
       , all (view open) doors -> Nothing
       | otherwise -> pure Stop
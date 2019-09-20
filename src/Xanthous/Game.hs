{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Game
  ( GameState(..)
  , entities
  , revealedPositions
  , messageHistory
  , randomGen
  , promptState
  , GamePromptState(..)

  , getInitialState

  , positionedCharacter
  , character
  , characterPosition
  , updateCharacterVision

  , MessageHistory(..)
  , pushMessage
  , popMessage
  , hideMessage

    -- * collisions
  , Collision(..)
  , collisionAt

    -- * App monad
  , AppT(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.List.NonEmpty ( NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           Control.Monad.State.Class
import           Control.Monad.State
import           Control.Monad.Random.Class
--------------------------------------------------------------------------------
import           Xanthous.Data.EntityMap (EntityMap, EntityID)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Data.EntityMap.Graphics
import           Xanthous.Data (Positioned, Position(..), positioned, position)
import           Xanthous.Entities
                 (SomeEntity(..), downcastEntity, entityIs, _SomeEntity)
import           Xanthous.Entities.Character
import           Xanthous.Entities.Creature
import           Xanthous.Entities.Item
import           Xanthous.Entities.Environment
import           Xanthous.Entities.Arbitrary ()
import           Xanthous.Orphans ()
import           Xanthous.Game.Prompt
--------------------------------------------------------------------------------

data MessageHistory
  = NoMessageHistory
  | MessageHistory (NonEmpty Text) Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Arbitrary MessageHistory where
  arbitrary = genericArbitrary

pushMessage :: Text -> MessageHistory -> MessageHistory
pushMessage msg NoMessageHistory = MessageHistory (msg :| []) True
pushMessage msg (MessageHistory msgs _) = MessageHistory (NonEmpty.cons msg msgs) True

popMessage :: MessageHistory -> MessageHistory
popMessage NoMessageHistory = NoMessageHistory
popMessage (MessageHistory msgs False) = MessageHistory msgs True
popMessage (MessageHistory msgs@(_ :| []) _) = MessageHistory msgs True
popMessage (MessageHistory (_ :| (msg : msgs)) True) = MessageHistory (msg :| msgs) True

hideMessage :: MessageHistory -> MessageHistory
hideMessage NoMessageHistory = NoMessageHistory
hideMessage (MessageHistory msgs _) = MessageHistory msgs False

--------------------------------------------------------------------------------

data GamePromptState m where
  NoPrompt :: GamePromptState m
  WaitingPrompt :: Text -> Prompt m -> GamePromptState m
  deriving stock (Show)

--------------------------------------------------------------------------------

newtype AppT m a
  = AppT { unAppT :: StateT GameState m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GameState
           )
       via (StateT GameState m)

--------------------------------------------------------------------------------

data GameState = GameState
  { _entities          :: !(EntityMap SomeEntity)
  , _revealedPositions :: !(Set Position)
  , _characterEntityID :: !EntityID
  , _messageHistory    :: !MessageHistory
  , _randomGen         :: !StdGen
  , _promptState       :: !(GamePromptState (AppT Identity))
  }
  deriving stock (Show)
makeLenses ''GameState

instance Eq GameState where
  (==) = (==) `on` \gs ->
    ( gs ^. entities
    , gs ^. revealedPositions
    , gs ^. characterEntityID
    , gs ^. messageHistory
    )


instance Arbitrary GameState where
  arbitrary = do
    char <- arbitrary @Character
    charPos <- arbitrary
    _messageHistory <- arbitrary
    (_characterEntityID, _entities) <- arbitrary <&>
      EntityMap.insertAtReturningID charPos (SomeEntity char)
    _revealedPositions <- fmap setFromList . sublistOf $ EntityMap.positions _entities
    _randomGen <- mkStdGen <$> arbitrary
    let _promptState = NoPrompt -- TODO
    pure $ GameState {..}


getInitialState :: IO GameState
getInitialState = do
  _randomGen <- getStdGen
  let char = mkCharacter
      (_characterEntityID, _entities)
        = EntityMap.insertAtReturningID
          (Position 0 0)
          (SomeEntity char)
          mempty
      _messageHistory = NoMessageHistory
      _revealedPositions = mempty
      _promptState = NoPrompt
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


--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

instance MonadTrans AppT where
  lift = AppT . lift

instance (Monad m) => MonadRandom (AppT m) where
  getRandomR rng = randomGen %%= randomR rng
  getRandom = randomGen %%= random
  getRandomRs rng = uses randomGen $ randomRs rng
  getRandoms = uses randomGen randoms

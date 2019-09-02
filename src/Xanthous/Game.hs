{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Xanthous.Game
  ( GameState(..)
  , entities
  , messageHistory
  , randomGen

  , getInitialState

  , positionedCharacter
  , character
  , characterPosition

  , MessageHistory(..)
  , pushMessage
  , popMessage
  , hideMessage
  ) where

import           Data.List.NonEmpty ( NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           Xanthous.Prelude

import           Xanthous.Data.EntityMap (EntityMap, EntityID)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Data (Positioned, Position(..), positioned, position)
import           Xanthous.Entities.SomeEntity
import           Xanthous.Entities.Character
import           Xanthous.Orphans ()

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

data GameState = GameState
  { _entities          :: EntityMap SomeEntity
  , _characterEntityID :: EntityID
  , _messageHistory    :: MessageHistory
  , _randomGen         :: StdGen
  }
  deriving stock (Show)
makeLenses ''GameState

instance Eq GameState where
  (GameState es₁ ceid₁ mh₁ _) == (GameState es₂ ceid₂ mh₂ _)
    = es₁ == es₂
    && ceid₁ == ceid₂
    && mh₁ == mh₂

instance Arbitrary GameState where
  arbitrary = do
    char <- arbitrary @Character
    charPos <- arbitrary
    _messageHistory <- arbitrary
    (_characterEntityID, _entities) <- arbitrary <&>
      EntityMap.insertAtReturningID charPos (SomeEntity char)
    _randomGen <- mkStdGen <$> arbitrary
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

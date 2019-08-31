{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Xanthous.Game
  ( GameState(..)
  , entities
  , getInitialState

  , positionedCharacter
  , character
  , characterPosition
  ) where

import Xanthous.Prelude
import Test.QuickCheck.Arbitrary

import Xanthous.Data.EntityMap (EntityMap, EntityID)
import qualified Xanthous.Data.EntityMap as EntityMap
import Xanthous.Data (Positioned, Position(..), positioned, position)
import Xanthous.Entities
import Xanthous.Entities.SomeEntity
import Xanthous.Entities.Character

data GameState = GameState
  { _entities          :: EntityMap SomeEntity
  , _characterEntityID :: EntityID
  }
  deriving stock (Show, Eq)
makeLenses ''GameState

instance Arbitrary GameState where
  arbitrary = do
    ents <- arbitrary
    char <- arbitrary
    pure $ getInitialState
      & entities .~ ents
      & positionedCharacter .~ char

getInitialState :: GameState
getInitialState =
  let char = mkCharacter
      (_characterEntityID, _entities)
        = EntityMap.insertAtReturningID
          (Position 0 0)
          (SomeEntity char)
          mempty
  in GameState {..}

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

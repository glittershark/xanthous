module Xanthous.Game
  ( GameState(..)
  , getInitialState
  ) where

import Xanthous.Prelude

data GameState = GameState
  { }

getInitialState :: IO GameState
getInitialState = pure GameState

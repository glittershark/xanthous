module Xanthous.Resource
  ( Name(..)
  ) where

import Xanthous.Prelude

data Name = MapViewport
            -- ^ The main viewport where we display the game content
          | Character
            -- ^ The character
          | MessageBox
            -- ^ The box where we display messages to the user
  deriving stock (Show, Eq, Ord)

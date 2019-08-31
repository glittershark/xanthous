module Xanthous.Command where

import Graphics.Vty.Input (Key(..), Modifier(..))

import Xanthous.Prelude hiding (Left, Right, Down)
import Xanthous.Data (Direction(..))

data Command
  = Quit
  | Move Direction
  | PickUp
  | PreviousMessage

commandFromKey :: Key -> [Modifier] -> Maybe Command
commandFromKey (KChar 'q') [] = Just Quit
commandFromKey (KChar 'h') [] = Just $ Move Left
commandFromKey (KChar 'j') [] = Just $ Move Down
commandFromKey (KChar 'k') [] = Just $ Move Up
commandFromKey (KChar 'l') [] = Just $ Move Right
commandFromKey _ _ = Nothing

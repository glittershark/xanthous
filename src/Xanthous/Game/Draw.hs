module Xanthous.Game.Draw
  ( drawGame
  ) where

import Xanthous.Prelude
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Xanthous.Game (GameState(..))
import Xanthous.Resource (Name(..))

drawMessages :: GameState -> Widget Name
drawMessages _ = str "Welcome to Xanthous! It's dangerous out there, why not stay inside?"

drawMap :: GameState -> Widget Name
drawMap _game
  = viewport MapViewport Both
  $ vBox mapRows
  where
    -- TODO
    firstRow = [str "@"] <> replicate 79 (str " ")
    mapRows = firstRow <> (replicate 20 . hBox . replicate 80 $ str " ")

drawGame :: GameState -> [Widget Name]
drawGame game = pure . withBorderStyle unicode
  $   drawMessages game
  <=> border (drawMap game)

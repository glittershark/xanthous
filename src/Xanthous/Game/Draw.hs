{-# LANGUAGE ViewPatterns #-}

module Xanthous.Game.Draw
  ( drawGame
  ) where

import Xanthous.Prelude
import Brick hiding (loc)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Xanthous.Data (Position(Position), x, y, loc)
import Xanthous.Data.EntityMap
import Xanthous.Entities
import Xanthous.Game (GameState(..), entities, characterPosition)
import Xanthous.Resource (Name(..))

drawMessages :: GameState -> Widget Name
drawMessages _ = str "Welcome to Xanthous! It's dangerous out there, why not stay inside?"

drawEntities :: (Draw a, Show a) => EntityMap a -> Widget Name
drawEntities em@(fromNullable . positions -> Just entityPositions)
  = vBox rows
  where
    maxPosition = maximum entityPositions
    maxY = maxPosition ^. y
    maxX = maxPosition ^. x
    rows = mkRow <$> [0..maxY]
    mkRow rowY = hBox $ renderEntityAt . flip Position rowY <$> [0..maxX]
    renderEntityAt pos = maybe (str " ") draw $ em ^? atPosition pos . folded
drawEntities _ = emptyWidget

drawMap :: GameState -> Widget Name
drawMap game
  = viewport MapViewport Both
  . showCursor Character (game ^. characterPosition . loc)
  . drawEntities
  $ game ^. entities

drawGame :: GameState -> [Widget Name]
drawGame game
  = pure
  . withBorderStyle unicode
  $   drawMessages game
  <=> border (drawMap game)

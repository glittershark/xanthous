{-# LANGUAGE ViewPatterns #-}

module Xanthous.Game.Draw
  ( drawGame
  ) where

import Xanthous.Prelude
import Brick hiding (loc)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Data.List.NonEmpty(NonEmpty((:|)))

import Xanthous.Data (Position(Position), x, y, loc)
import Xanthous.Data.EntityMap
import Xanthous.Entities
import Xanthous.Game
  ( GameState(..)
  , entities
  , characterPosition
  , MessageHistory(..)
  , messageHistory
  )
import Xanthous.Resource (Name(..))
import Xanthous.Orphans ()

drawMessages :: MessageHistory -> Widget Name
drawMessages NoMessageHistory = emptyWidget
drawMessages (MessageHistory _ False) = emptyWidget
drawMessages (MessageHistory (lastMessage :| _) True) = txt lastMessage

-- an attempt to still take up a row even when no messages
-- drawMessages msgs = vLimit 1 . Widget Greedy Fixed . render $ case msgs of
--   NoMessageHistory -> padTop (Pad 2) $ str " "
--   (MessageHistory _ False) -> padTop (Pad 2) $ str " "
--   (MessageHistory (lastMessage :| _) True) -> txt lastMessage

drawEntities :: (Draw a, Show a) => EntityMap a -> Widget Name
drawEntities em
  = vBox rows
  where
    entityPositions = positions em
    maxY = fromMaybe 0 $ maximumOf (folded . y) entityPositions
    maxX = fromMaybe 0 $ maximumOf (folded . x) entityPositions
    rows = mkRow <$> [0..maxY]
    mkRow rowY = hBox $ renderEntityAt . flip Position rowY <$> [0..maxX]
    renderEntityAt pos = maybe (str " ") draw $ em ^? atPosition pos . folded

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
  $   drawMessages (game ^. messageHistory)
  <=> border (drawMap game)

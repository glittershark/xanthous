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
import Xanthous.Data.EntityMap (EntityMap, atPosition)
import qualified Xanthous.Data.EntityMap as EntityMap
import Xanthous.Entities
import Xanthous.Game
  ( GameState(..)
  , entities
  , revealedPositions
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

drawEntities
  :: Set Position
    -- ^ Positions the character has seen
    -- FIXME: this will break down as soon as creatures can walk around on their
    -- own, since we don't want to render things walking around when the
    -- character can't see them
  -> EntityMap SomeEntity -- ^ all entities
  -> Widget Name
drawEntities visiblePositions allEnts
  = vBox rows
  where
    entityPositions = EntityMap.positions allEnts
    maxY = fromMaybe 0 $ maximumOf (folded . y) entityPositions
    maxX = fromMaybe 0 $ maximumOf (folded . x) entityPositions
    rows = mkRow <$> [0..maxY]
    mkRow rowY = hBox $ renderEntityAt . flip Position rowY <$> [0..maxX]
    renderEntityAt pos
      | pos `member` visiblePositions
      = let neighbors = EntityMap.neighbors pos allEnts
        in maybe (str " ") (drawWithNeighbors neighbors)
           $ allEnts ^? atPosition pos . folded
      | otherwise = str " "

drawMap :: GameState -> Widget Name
drawMap game
  = viewport MapViewport Both
  . showCursor Character (game ^. characterPosition . loc)
  $ drawEntities
    (game ^. revealedPositions)
    (game ^. entities)

drawGame :: GameState -> [Widget Name]
drawGame game
  = pure
  . withBorderStyle unicode
  $   drawMessages (game ^. messageHistory)
  <=> border (drawMap game)

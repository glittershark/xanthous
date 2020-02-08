{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityMap.Graphics
  ( visiblePositions
  , visibleEntities
  , linesOfSight
  , canSee
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude hiding (lines)
--------------------------------------------------------------------------------
import Xanthous.Util (takeWhileInclusive)
import Xanthous.Data
import Xanthous.Data.Entities
import Xanthous.Data.EntityMap
import Xanthous.Game.State
import Xanthous.Util.Graphics (circle, line)
--------------------------------------------------------------------------------

visiblePositions :: Entity e => Position -> Word -> EntityMap e -> Set Position
visiblePositions pos radius = setFromList . positions . visibleEntities pos radius

-- | Returns a list of individual lines of sight, each of which is a list of
-- entities at positions on that line of sight
linesOfSight
  :: forall e. Entity e
  => Position
  -> Word
  -> EntityMap e
  -> [[(Position, Vector (EntityID, e))]]
linesOfSight (view _Position -> pos) visionRadius em
  = entitiesOnLines
  <&> takeWhileInclusive
      (none (view blocksVision . entityAttributes . snd) . snd)
  where
    radius = circle pos $ fromIntegral visionRadius
    lines = line pos <$> radius
    entitiesOnLines :: [[(Position, Vector (EntityID, e))]]
    entitiesOnLines = lines <&> map getPositionedAt
    getPositionedAt :: (Int, Int) -> (Position, Vector (EntityID, e))
    getPositionedAt p =
      let ppos = _Position # p
      in (ppos, over _2 (view positioned) <$> atPositionWithIDs ppos em)

-- | Given a point and a radius of vision, returns a list of all entities that
-- are *visible* (eg, not blocked by an entity that obscures vision) from that
-- point
visibleEntities :: Entity e => Position -> Word -> EntityMap e -> EntityMap e
visibleEntities pos visionRadius
  = fromEIDsAndPositioned
  . fold
  . map (\(p, es) -> over _2 (Positioned p) <$> es)
  . fold
  . linesOfSight pos visionRadius

canSee :: Entity e => (e -> Bool) -> Position -> Word -> EntityMap e -> Bool
canSee match pos radius = any match . visibleEntities pos radius
-- ^ this might be optimizable

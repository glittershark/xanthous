{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityMap.Graphics where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Xanthous.Util (takeWhileInclusive)
import Xanthous.Data
import Xanthous.Data.EntityMap
import Xanthous.Entities
import Xanthous.Util.Graphics (circle, line)
--------------------------------------------------------------------------------

-- | Given a point and a radius of vision, returns a list of all entities that
-- are *visible* (eg, not blocked by an entity that obscures vision) from that
-- point
visibleEntities :: Position -> Word -> EntityMap SomeEntity -> EntityMap SomeEntity
visibleEntities (view _Position -> pos) visionRadius em
  = fromEIDsAndPositioned . fold . fold $ sightAdjustedLines
  where
    -- I love laziness!
    radius = circle pos $ fromIntegral visionRadius
    linesOfSight = radius <&> line pos
    entitiesOnLines = linesOfSight <&> map getPositionedAt
    sightAdjustedLines = entitiesOnLines <&> takeWhileInclusive (none $ blocksVision . snd)
    getPositionedAt p =
      let ppos = _Position # p
      in atPositionWithIDs ppos em

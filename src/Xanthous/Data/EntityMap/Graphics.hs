--------------------------------------------------------------------------------
module Xanthous.Data.EntityMap.Graphics
  ( visiblePositions
  , visibleEntities
  , lineOfSight
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

-- | Returns a set of positions that are visible, when taking into account
-- 'blocksVision', from the given position, within the given radius.
visiblePositions
  :: Entity e
  => Position
  -> Word -- ^ Vision radius
  -> EntityMap e
  -> Set Position
visiblePositions pos radius
  = setFromList . positions . visibleEntities pos radius

-- | Returns a list of entities on the *line of sight* from the first position
-- to the second position
lineOfSight
  :: forall e. Entity e
  => Position -- ^ Origin
  -> Position -- ^ Destination
  -> EntityMap e
  -> [(Position, Vector (EntityID, e))]
lineOfSight (view _Position -> origin) (view _Position -> destination) em =
  takeWhileInclusive (none (view blocksVision . entityAttributes . snd) . snd)
    $ getPositionedAt <$> line origin destination
  where
    getPositionedAt :: V2 Int -> (Position, Vector (EntityID, e))
    getPositionedAt (review _Position -> p) =
      (p, over _2 (view positioned) <$> atPositionWithIDs p em)

-- | Returns a list of individual lines of sight, each of which is a list of
-- entities at positions on that line of sight
linesOfSight
  :: forall e. Entity e
  => Position    -- ^ Centerpoint
  -> Word        -- ^ Radius
  -> EntityMap e
  -> [[(Position, Vector (EntityID, e))]]
linesOfSight pos visionRadius em =
  radius <&> \edge -> lineOfSight pos (_Position # edge) em
  where
    radius = circle (pos ^. _Position) $ fromIntegral visionRadius

-- | Given a point and a radius of vision, returns a list of all entities that
-- are *visible* (eg, not blocked by an entity that obscures vision) from that
-- point
visibleEntities :: Entity e => Position -> Word -> EntityMap e -> EntityMap e
visibleEntities pos visionRadius
  = fromEIDsAndPositioned
  . foldMap (\(p, es) -> over _2 (Positioned p) <$> es)
  . fold
  . linesOfSight pos visionRadius

canSee :: Entity e => (e -> Bool) -> Position -> Word -> EntityMap e -> Bool
canSee match pos radius = any match . visibleEntities pos radius
-- ^ this might be optimizable

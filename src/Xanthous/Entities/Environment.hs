module Xanthous.Entities.Environment
  ( Wall(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Brick (str)
import Brick.Widgets.Border.Style (unicode)
--------------------------------------------------------------------------------
import Xanthous.Entities (Draw(..), entityIs, Entity(..))
import Xanthous.Entities.Draw.Util
import Xanthous.Data
--------------------------------------------------------------------------------

data Wall = Wall
  deriving stock (Show, Eq, Ord, Generic, Enum)
  deriving anyclass (CoArbitrary, Function)

instance Entity Wall where
  blocksVision _ = True

instance Arbitrary Wall where
  arbitrary = pure Wall

instance Draw Wall where
  drawWithNeighbors neighs _wall =
    str . pure . borderFromEdges unicode $ wallEdges
    where
      wallEdges = any (entityIs @Wall) <$> edges neighs

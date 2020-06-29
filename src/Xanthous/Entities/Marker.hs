--------------------------------------------------------------------------------
module Xanthous.Entities.Marker ( Marker(..) ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.Aeson
import           Test.QuickCheck
import qualified Graphics.Vty.Attributes as Vty
import qualified Graphics.Vty.Image as Vty
import           Brick.Widgets.Core (raw)
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Data.Entities (EntityAttributes(..))
--------------------------------------------------------------------------------

-- | Mark on the map - for use in debugging / development only.
newtype Marker = Marker Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (Semigroup, Monoid, ToJSON, FromJSON, Arbitrary) via Text

instance Brain Marker where step = brainVia Brainless

instance Entity Marker where
  entityAttributes = const EntityAttributes
    { _blocksVision = False
    , _blocksObject = False
    , _collision = Stop
    }
  description (Marker m) = "[M] " <> m
  entityChar = const $ "X" & style .~ markerStyle
  entityCollision = const Nothing

instance Draw Marker where
  draw = const . raw $ Vty.char markerStyle 'X'
  drawPriority = const maxBound

markerStyle :: Vty.Attr
markerStyle = Vty.defAttr
  `Vty.withForeColor` Vty.red
  `Vty.withBackColor` Vty.black

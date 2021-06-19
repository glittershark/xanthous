--------------------------------------------------------------------------------
module Xanthous.Data.App
  ( Panel(..)
  , ResourceName(..)
  , AppEvent(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import Xanthous.Util.QuickCheck
--------------------------------------------------------------------------------

-- | Enum for "panels" displayed in the game's UI.
data Panel
  = -- | A panel displaying the character's inventory
    InventoryPanel
  | -- | A panel describing an item in the inventory in detail
    --
    -- The argument is the full description of the item
    ItemDescriptionPanel Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function, ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary Panel


data ResourceName
  = MapViewport -- ^ The main viewport where we display the game content
  | Character   -- ^ The character
  | MessageBox  -- ^ The box where we display messages to the user
  | Prompt      -- ^ The game's prompt
  | Panel Panel -- ^ A panel in the game
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function, ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary ResourceName

data AppEvent
  = AutoContinue -- ^ Continue whatever autocommand has been requested by the
                 --   user
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function, ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary AppEvent

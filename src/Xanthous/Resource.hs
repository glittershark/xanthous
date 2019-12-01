--------------------------------------------------------------------------------
module Xanthous.Resource
  ( Panel(..)
  , Name(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Test.QuickCheck
import Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------
import Xanthous.Util.QuickCheck
--------------------------------------------------------------------------------

-- | Enum for "panels" displayed in the game's UI.
data Panel
  = InventoryPanel -- ^ A panel displaying the character's inventory
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFData, CoArbitrary, Function, ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary Panel


data Name
  = MapViewport -- ^ The main viewport where we display the game content
  | Character   -- ^ The character
  | MessageBox  -- ^ The box where we display messages to the user
  | Prompt      -- ^ The game's prompt
  | Panel Panel -- ^ A panel in the game
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function, ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary Name

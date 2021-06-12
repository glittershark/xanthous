{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- | Memoized versions of calculations
--------------------------------------------------------------------------------
module Xanthous.Game.Memo
  ( MemoState
  , emptyMemoState
  , clear
    -- ** Memo lenses
  , characterVisiblePositions

    -- * Memoized values
  , Memoized(UnMemoized)
  , memoizeWith
  , getMemoized
  , runMemoized
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Generic.DerivingVia
import Test.QuickCheck (CoArbitrary, Function, Arbitrary)
--------------------------------------------------------------------------------
import Xanthous.Data (Position)
import Xanthous.Data.Memo
import Xanthous.Util.QuickCheck (GenericArbitrary(GenericArbitrary))
--------------------------------------------------------------------------------

-- | Memoized calculations on the game state
data MemoState = MemoState
  { -- | Memoized version of 'Xanthous.Game.Lenses.characterVisiblePositions',
    -- memoized with the position of the character
    _characterVisiblePositions :: Memoized Position (Set Position)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary MemoState
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           MemoState
makeLenses ''MemoState

emptyMemoState :: MemoState
emptyMemoState = MemoState { _characterVisiblePositions = UnMemoized }
{-# INLINE emptyMemoState #-}

clear :: Lens' MemoState (Memoized k v) -> MemoState -> MemoState
clear = flip set UnMemoized
{-# INLINE clear #-}

{-# ANN module ("Hlint: ignore Use newtype instead of data" :: String) #-}

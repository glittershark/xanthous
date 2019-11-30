--------------------------------------------------------------------------------
module Xanthous.Prelude
  ( module ClassyPrelude
  , Type
  , Constraint
  , module GHC.TypeLits
  , module Control.Lens
  , module Data.Void
  , module Control.Comonad
  ) where
--------------------------------------------------------------------------------
import ClassyPrelude hiding
  (return, (<|), unsnoc, uncons, cons, snoc, index, (<.>), Index, say)
import Data.Kind
import GHC.TypeLits hiding (Text)
import Control.Lens
import Data.Void
import Control.Comonad
--------------------------------------------------------------------------------

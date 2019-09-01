module Xanthous.Prelude
  ( module ClassyPrelude
  , Type
  , Constraint
  , module GHC.TypeLits
  , module Control.Lens
  , module Data.Void
  ) where

import ClassyPrelude hiding
  (return, (<|), unsnoc, uncons, cons, snoc, index, (<.>), Index)
import Data.Kind
import GHC.TypeLits hiding (Text)
import Control.Lens
import Data.Void

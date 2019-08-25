module Xanthous.Prelude
  ( module ClassyPrelude
  , Type
  , Constraint
  , module GHC.TypeLits
  ) where

import ClassyPrelude hiding (return)
import Data.Kind
import GHC.TypeLits hiding (Text)

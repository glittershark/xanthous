module Xanthous.Util
  ( EqEqProp(..)
  , EqProp(..)
  ) where

import Xanthous.Prelude

import Test.QuickCheck.Checkers

newtype EqEqProp a = EqEqProp a
  deriving newtype Eq

instance Eq a => EqProp (EqEqProp a) where
  (=-=) = eq

module Test.Prelude
  ( module Xanthous.Prelude
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.QuickCheck
  , module Test.QuickCheck.Classes
  , testBatch
  ) where

import Xanthous.Prelude hiding (assert, elements)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers (TestBatch)
import Test.QuickCheck.Instances.ByteString ()

testBatch :: TestBatch -> TestTree
testBatch (name, tests) = testGroup name $ uncurry testProperty <$> tests

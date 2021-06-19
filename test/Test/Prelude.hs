{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
module Test.Prelude
  ( module Xanthous.Prelude
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.QuickCheck
  , module Test.QuickCheck.Classes
  , testBatch
  , jsonRoundTrip
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (assert, elements)
--------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Checkers (TestBatch, EqProp ((=-=)))
import           Test.QuickCheck.Instances.ByteString ()
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
import           Data.Aeson (ToJSON, FromJSON)
--------------------------------------------------------------------------------

testBatch :: TestBatch -> TestTree
testBatch (name, tests) = testGroup name $ uncurry testProperty <$> tests

jsonRoundTrip
  :: forall a. (ToJSON a, FromJSON a, EqProp a, Arbitrary a, Show a) => TestTree
jsonRoundTrip = testProperty "JSON round trip" $ \(x :: a) ->
  JSON.decode (JSON.encode x) =-= Just x

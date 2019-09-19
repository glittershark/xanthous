{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Arbitrary () where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen
--------------------------------------------------------------------------------
import           Xanthous.Entities (SomeEntity(..))
import           Xanthous.Entities.Character
import           Xanthous.Entities.Environment
--------------------------------------------------------------------------------

instance Arbitrary SomeEntity where
  arbitrary = Gen.oneof
    [ SomeEntity <$> arbitrary @Character
    , pure $ SomeEntity Wall
    ]

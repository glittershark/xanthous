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
import           Xanthous.Entities.Item
import           Xanthous.Entities.Creature
import           Xanthous.Entities.Environment
import           Xanthous.AI.Gormlak ()
--------------------------------------------------------------------------------

instance Arbitrary SomeEntity where
  arbitrary = Gen.oneof
    [ SomeEntity <$> arbitrary @Character
    , SomeEntity <$> arbitrary @Item
    , SomeEntity <$> arbitrary @Creature
    , SomeEntity <$> arbitrary @Wall
    , SomeEntity <$> arbitrary @Door
    ]

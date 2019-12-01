{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Entities () where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen
import           Data.Aeson
--------------------------------------------------------------------------------
import           Xanthous.Entities.Character
import           Xanthous.Entities.Item
import           Xanthous.Entities.Creature
import           Xanthous.Entities.Environment
import           Xanthous.Game.State
import           {-# SOURCE #-} Xanthous.AI.Gormlak ()
import           Xanthous.Util.QuickCheck
import           Data.Aeson.Generic.DerivingVia
--------------------------------------------------------------------------------

instance Arbitrary SomeEntity where
  arbitrary = Gen.oneof
    [ SomeEntity <$> arbitrary @Character
    , SomeEntity <$> arbitrary @Item
    , SomeEntity <$> arbitrary @Creature
    , SomeEntity <$> arbitrary @Wall
    , SomeEntity <$> arbitrary @Door
    , SomeEntity <$> arbitrary @GroundMessage
    ]

instance FromJSON SomeEntity where
  parseJSON = withObject "Entity" $ \obj -> do
    (entityType :: Text) <- obj .: "type"
    case entityType of
      "Character" -> SomeEntity @Character <$> obj .: "data"
      "Item" -> SomeEntity @Item <$> obj .: "data"
      "Creature" -> SomeEntity @Creature <$> obj .: "data"
      "Wall" -> SomeEntity @Wall <$> obj .: "data"
      "Door" -> SomeEntity @Door <$> obj .: "data"
      "GroundMessage" -> SomeEntity @GroundMessage <$> obj .: "data"
      _ -> fail . unpack $ "Invalid entity type \"" <> entityType <> "\""

deriving via WithOptions '[ FieldLabelModifier '[Drop 1] ] GameState
  instance FromJSON GameState

instance Entity SomeEntity where
  blocksVision (SomeEntity ent) = blocksVision ent
  description (SomeEntity ent) = description ent
  entityChar (SomeEntity ent) = entityChar ent

instance Function SomeEntity where
  function = functionJSON

instance CoArbitrary SomeEntity where
  coarbitrary = coarbitrary . encode

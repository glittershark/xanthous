{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Data.Entities
  ( -- * Collisions
    Collision(..)
  , _Stop
  , _Combat
    -- * Entity Attributes
  , EntityAttributes(..)
  , blocksVision
  , blocksObject
  , collision
  , defaultEntityAttributes
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.Aeson (ToJSON(..), FromJSON(..), (.:?), (.!=), withObject)
import           Data.Aeson.Generic.DerivingVia
import           Xanthous.Util.QuickCheck (GenericArbitrary(..))
import           Test.QuickCheck
--------------------------------------------------------------------------------

data Collision
  = Stop   -- ^ Can't move through this
  | Combat -- ^ Moving into this equates to hitting it with a stick
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Collision
  deriving (ToJSON, FromJSON)
       via WithOptions '[ AllNullaryToStringTag 'True ]
           Collision
makePrisms ''Collision

-- | Attributes of an entity
data EntityAttributes = EntityAttributes
  { _blocksVision :: Bool
    -- | Does this entity block a large object from being put in the same tile as
    -- it - eg a a door being closed on it
  , _blocksObject :: Bool
    -- | What type of collision happens when moving into this entity?
  , _collision :: Collision
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary EntityAttributes
  deriving (ToJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           EntityAttributes
makeLenses ''EntityAttributes

instance FromJSON EntityAttributes where
  parseJSON = withObject "EntityAttributes" $ \o -> do
    _blocksVision <- o .:? "blocksVision"
                      .!= _blocksVision defaultEntityAttributes
    _blocksObject <- o .:? "blocksObject"
                      .!= _blocksObject defaultEntityAttributes
    _collision    <- o .:? "collision"
                      .!= _collision defaultEntityAttributes
    pure EntityAttributes {..}

defaultEntityAttributes :: EntityAttributes
defaultEntityAttributes = EntityAttributes
  { _blocksVision = False
  , _blocksObject = False
  , _collision    = Stop
  }

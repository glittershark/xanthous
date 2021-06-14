{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Creature.Hippocampus
  (-- * Hippocampus
    Hippocampus(..)
  , initialHippocampus
    -- ** Lenses
  , destination
  , greetedCharacter
    -- ** Destination
  , Destination(..)
  , destinationFromPos
    -- *** Lenses
  , destinationPosition
  , destinationProgress
  )
where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.Aeson.Generic.DerivingVia
import           Data.Aeson (ToJSON, FromJSON)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------
import           Xanthous.Data
import           Xanthous.Util.QuickCheck
--------------------------------------------------------------------------------


data Destination = Destination
  { _destinationPosition :: !Position
    -- | The progress towards the destination, tracked as an offset from the
    -- creature's original position.
    --
    -- When this value reaches >= 1, the creature has reached their destination
  , _destinationProgress :: !Tiles
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Destination
instance Arbitrary Destination where arbitrary = genericArbitrary
makeLenses ''Destination

destinationFromPos :: Position -> Destination
destinationFromPos _destinationPosition =
  let _destinationProgress = 0
  in Destination{..}

data Hippocampus = Hippocampus
  { _destination      :: !(Maybe Destination)
  , -- | Has this creature greeted the character in any way yet?
    --
    -- Some creature types ignore this field
    _greetedCharacter :: !Bool
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Hippocampus
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       Hippocampus
makeLenses ''Hippocampus

initialHippocampus :: Hippocampus
initialHippocampus = Hippocampus
  { _destination      = Nothing
  , _greetedCharacter = False
  }

{-# LANGUAGE TemplateHaskell #-}
module Xanthous.Entities.Environment
  (
    -- * Walls
    Wall(..)
    -- * Doors
  , Door(..)
  , open
  , locked
  , unlockedDoor
    -- * Messages
  , GroundMessage(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Test.QuickCheck
import Brick (str)
import Brick.Widgets.Border.Style (unicode)
import Brick.Types (Edges(..))
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
--------------------------------------------------------------------------------
import Xanthous.Entities.Draw.Util
import Xanthous.Data
import Xanthous.Game.State
import Xanthous.Util.QuickCheck
--------------------------------------------------------------------------------

data Wall = Wall
  deriving stock (Show, Eq, Ord, Generic, Enum)
  deriving anyclass (NFData, CoArbitrary, Function)

instance ToJSON Wall where
  toJSON = const $ String "Wall"

instance FromJSON Wall where
  parseJSON = withText "Wall" $ \case
    "Wall" -> pure Wall
    _      -> fail "Invalid Wall: expected Wall"

instance Brain Wall where step = brainVia Brainless

instance Entity Wall where
  blocksVision _ = True
  description _ = "a wall"
  entityChar _ = "┼"

instance Arbitrary Wall where
  arbitrary = pure Wall

wallEdges :: (MonoFoldable mono, Element mono ~ SomeEntity)
          => Neighbors mono -> Edges Bool
wallEdges neighs = any (entityIs @Wall) <$> edges neighs

instance Draw Wall where
  drawWithNeighbors neighs _wall =
    str . pure . borderFromEdges unicode $ wallEdges neighs

data Door = Door
  { _open   :: Bool
  , _locked :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function, ToJSON, FromJSON)
  deriving Arbitrary via GenericArbitrary Door
makeLenses ''Door

instance Draw Door where
  drawWithNeighbors neighs door
    = str . pure . ($ door ^. open) $ case wallEdges neighs of
        Edges True  False  False False -> vertDoor
        Edges False True   False False -> vertDoor
        Edges True  True   False False -> vertDoor
        Edges False False  True  False -> horizDoor
        Edges False False  False True  -> horizDoor
        Edges False False  True  True  -> horizDoor
        _                              -> allsidesDoor
    where
      horizDoor True = '␣'
      horizDoor False = 'ᚔ'
      vertDoor True = '['
      vertDoor False = 'ǂ'
      allsidesDoor True = '+'
      allsidesDoor False = '▥'

instance Brain Door where step = brainVia Brainless

instance Entity Door where
  blocksVision = not . view open
  description door | door ^. open = "an open door"
                   | otherwise    = "a closed door"
  entityChar _ = "d"
  entityCollision door | door ^. open = Nothing
                       | otherwise = Just Stop

-- | A closed, unlocked door
unlockedDoor :: Door
unlockedDoor = Door
  { _open = False
  , _locked = False
  }

--------------------------------------------------------------------------------

newtype GroundMessage = GroundMessage Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary GroundMessage
  deriving (ToJSON, FromJSON)
       via WithOptions '[ 'TagSingleConstructors 'True
                        , 'SumEnc 'ObjWithSingleField
                        ]
           GroundMessage
  deriving Draw
       via DrawStyledCharacter ('Just 'Yellow) 'Nothing "≈"
           GroundMessage
instance Brain GroundMessage where step = brainVia Brainless

instance Entity GroundMessage where
  blocksVision = const False
  description = const "a message on the ground. Press r. to read it."
  entityChar = const "≈"
  entityCollision = const Nothing

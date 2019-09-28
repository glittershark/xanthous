{-# LANGUAGE TemplateHaskell #-}
module Xanthous.Entities.Environment
  ( Wall(..)
  , Door(..)
  , open
  , locked
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Brick (str)
import Brick.Widgets.Border.Style (unicode)
import Brick.Types (Edges(..))
--------------------------------------------------------------------------------
import Xanthous.Entities
       ( Draw(..)
       , entityIs
       , Entity(..)
       , SomeEntity
       , Brain(..)
       , Brainless(..)
       , brainVia
       )
import Xanthous.Entities.Draw.Util
import Xanthous.Data
--------------------------------------------------------------------------------

data Wall = Wall
  deriving stock (Show, Eq, Ord, Generic, Enum)
  deriving anyclass (CoArbitrary, Function)

-- deriving via Brainless Wall instance Brain Wall
instance Brain Wall where step = brainVia Brainless

instance Entity Wall where
  blocksVision _ = True
  description _ = "a wall"

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
  deriving anyclass (NFData, CoArbitrary, Function)
makeLenses ''Door

instance Arbitrary Door where
  arbitrary = genericArbitrary

instance Draw Door where
  drawWithNeighbors neighs door
    | door ^. open
    = str . pure $ case wallEdges neighs of
        Edges True  False  False False -> vertDoor
        Edges False True   False False -> vertDoor
        Edges True  True   False False -> vertDoor
        Edges False False  True  False -> horizDoor
        Edges False False  False True  -> horizDoor
        Edges False False  True  True  -> horizDoor
        _                              -> '+'
    | otherwise    = str "\\"
    where
      horizDoor = '␣'
      vertDoor = '['

-- deriving via Brainless Door instance Brain Door
instance Brain Door where step = brainVia Brainless

instance Entity Door where
  blocksVision = not . view open
  description _ = "a door"

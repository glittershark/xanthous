{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- | Common data types for Xanthous
--------------------------------------------------------------------------------
module Xanthous.Data
  ( -- *
    Position(..)
  , x
  , y

  , Positioned(..)
  , _Positioned
  , position
  , positioned
  , loc
  , _Position
  , positionFromPair
  , addPositions
  , diffPositions
  , stepTowards
  , isUnit

    -- *
  , Dimensions'(..)
  , Dimensions
  , HasWidth(..)
  , HasHeight(..)

    -- *
  , Direction(..)
  , opposite
  , move
  , asPosition
  , directionOf

    -- *
  , Neighbors(..)
  , edges
  , neighborDirections
  , neighborPositions
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (Left, Down, Right)
import           Test.QuickCheck (Arbitrary, CoArbitrary, Function)
import           Test.QuickCheck.Arbitrary.Generic
import           Data.Group
import           Brick (Location(Location), Edges(..))
--------------------------------------------------------------------------------
import           Xanthous.Util (EqEqProp(..), EqProp)
import           Xanthous.Orphans ()
import           Xanthous.Util.Graphics
--------------------------------------------------------------------------------

data Position where
  Position :: { _x :: Int
             , _y :: Int
             } -> Position
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (Hashable, CoArbitrary, Function)
  deriving EqProp via EqEqProp Position
makeLenses ''Position

instance Arbitrary Position where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Semigroup Position where
  (Position x₁ y₁) <> (Position x₂ y₂) = Position (x₁ + x₂) (y₁ + y₂)

instance Monoid Position where
  mempty = Position 0 0

instance Group Position where
  invert (Position px py) = Position (-px) (-py)

data Positioned a where
  Positioned :: Position -> a -> Positioned a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving anyclass (CoArbitrary, Function)
type role Positioned representational

_Positioned :: Iso (Position, a) (Position, b) (Positioned a) (Positioned b)
_Positioned = iso hither yon
  where
    hither (pos, a) = Positioned pos a
    yon (Positioned pos b) = (pos, b)

instance Arbitrary a => Arbitrary (Positioned a) where
  arbitrary = Positioned <$> arbitrary <*> arbitrary

position :: Lens' (Positioned a) Position
position = lens
  (\(Positioned pos _) -> pos)
  (\(Positioned _ a) pos -> Positioned pos a)

positioned :: Lens (Positioned a) (Positioned b) a b
positioned = lens
  (\(Positioned _ x') -> x')
  (\(Positioned pos _) x' -> Positioned pos x')

loc :: Iso' Position Location
loc = iso hither yon
  where
    hither (Position px py) = Location (px, py)
    yon (Location (lx, ly)) = Position lx ly

_Position :: Iso' Position (Int, Int)
_Position = iso hither yon
  where
    hither (Position px py) = (px, py)
    yon (lx, ly) = Position lx ly

positionFromPair :: (Integral i, Integral j) => (i, j) -> Position
positionFromPair (i, j) = Position (fromIntegral i) (fromIntegral j)

-- | Add two positions
--
-- Operation for the additive group on positions
addPositions :: Position -> Position -> Position
addPositions = (<>)

-- | Subtract two positions.
--
-- diffPositions pos₁ pos₂ = pos₁ `addPositions` (invert pos₂)
diffPositions :: Position -> Position -> Position
diffPositions (Position x₁ y₁) (Position x₂ y₂) = Position (x₁ - x₂) (y₁ - y₂)

-- | Is this position a unit position? or: When taken as a difference, does this
-- position represent a step of one tile?
--
-- ∀ dir :: Direction. isUnit ('asPosition' dir)
isUnit :: Position -> Bool
isUnit (Position px py) = abs px == 1 || abs py == 1

--------------------------------------------------------------------------------

data Dimensions' a = Dimensions
  { _width :: a
  , _height :: a
  }
  deriving stock (Show, Eq, Functor, Generic)
  deriving anyclass (CoArbitrary, Function)
makeFieldsNoPrefix ''Dimensions'

instance Arbitrary a => Arbitrary (Dimensions' a) where
  arbitrary = Dimensions <$> arbitrary <*> arbitrary

type Dimensions = Dimensions' Word

--------------------------------------------------------------------------------

data Direction where
  Up        :: Direction
  Down      :: Direction
  Left      :: Direction
  Right     :: Direction
  UpLeft    :: Direction
  UpRight   :: Direction
  DownLeft  :: Direction
  DownRight :: Direction
  Here      :: Direction
  deriving stock (Show, Eq, Generic)

instance Arbitrary Direction where
  arbitrary = genericArbitrary
  shrink = genericShrink

opposite :: Direction -> Direction
opposite Up        = Down
opposite Down      = Up
opposite Left      = Right
opposite Right     = Left
opposite UpLeft    = DownRight
opposite UpRight   = DownLeft
opposite DownLeft  = UpRight
opposite DownRight = UpLeft
opposite Here      = Here

move :: Direction -> Position -> Position
move Up        = y -~ 1
move Down      = y +~ 1
move Left      = x -~ 1
move Right     = x +~ 1
move UpLeft    = move Up . move Left
move UpRight   = move Up . move Right
move DownLeft  = move Down . move Left
move DownRight = move Down . move Right
move Here      = id

asPosition :: Direction -> Position
asPosition dir = move dir mempty

-- | Returns the direction that a given position is from a given source position
directionOf
  :: Position -- ^ Source
  -> Position -- ^ Target
  -> Direction
directionOf (Position x₁ y₁) (Position x₂ y₂) =
  case (x₁ `compare` x₂, y₁ `compare` y₂) of
    (EQ, EQ) -> Here
    (EQ, LT) -> Down
    (EQ, GT) -> Up
    (LT, EQ) -> Right
    (GT, EQ) -> Left

    (LT, LT) -> DownRight
    (GT, LT) -> DownLeft

    (LT, GT) -> UpRight
    (GT, GT) -> UpLeft

-- | Take one (potentially diagonal) step towards the given position
--
-- ∀ src tgt. isUnit (src `diffPositions` (src `stepTowards tgt`))
stepTowards
  :: Position -- ^ Source
  -> Position -- ^ Target
  -> Position
stepTowards (view _Position -> p₁) (view _Position -> p₂)
  | p₁ == p₂ = _Position # p₁
  | otherwise =
    let (_:p:_) = line p₁ p₂
    in _Position # p

--------------------------------------------------------------------------------

data Neighbors a = Neighbors
  { _topLeft
  , _top
  , _topRight
  , _left
  , _right
  , _bottomLeft
  , _bottom
  , _bottomRight :: a
  }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)
makeLenses ''Neighbors

instance Applicative Neighbors where
  pure α = Neighbors
    { _topLeft     = α
    , _top         = α
    , _topRight    = α
    , _left        = α
    , _right       = α
    , _bottomLeft  = α
    , _bottom      = α
    , _bottomRight = α
    }
  nf <*> nx = Neighbors
    { _topLeft     = nf ^. topLeft     $ nx ^. topLeft
    , _top         = nf ^. top         $ nx ^. top
    , _topRight    = nf ^. topRight    $ nx ^. topRight
    , _left        = nf ^. left        $ nx ^. left
    , _right       = nf ^. right       $ nx ^. right
    , _bottomLeft  = nf ^. bottomLeft  $ nx ^. bottomLeft
    , _bottom      = nf ^. bottom      $ nx ^. bottom
    , _bottomRight = nf ^. bottomRight $ nx ^. bottomRight
    }

edges :: Neighbors a -> Edges a
edges neighs = Edges
  { eTop = neighs ^. top
  , eBottom = neighs ^. bottom
  , eLeft = neighs ^. left
  , eRight = neighs ^. right
  }

neighborDirections :: Neighbors Direction
neighborDirections = Neighbors
  { _topLeft     = UpLeft
  , _top         = Up
  , _topRight    = UpRight
  , _left        = Left
  , _right       = Right
  , _bottomLeft  = DownLeft
  , _bottom      = Down
  , _bottomRight = DownRight
  }

neighborPositions :: Position -> Neighbors Position
neighborPositions pos = (`move` pos) <$> neighborDirections

--------------------------------------------------------------------------------

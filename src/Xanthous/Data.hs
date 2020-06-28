{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE RoleAnnotations        #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE NoTypeSynonymInstances #-}
{-# LANGUAGE DuplicateRecordFields  #-}
--------------------------------------------------------------------------------
-- | Common data types for Xanthous
--------------------------------------------------------------------------------
module Xanthous.Data
  ( Opposite(..)

    -- *
  , Position'(..)
  , Position
  , x
  , y

    -- **
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

    -- * Boxes
  , Box(..)
  , topLeftCorner
  , bottomRightCorner
  , setBottomRightCorner
  , dimensions
  , inBox
  , boxIntersects
  , boxCenter
  , boxEdge
  , module Linear.V2

    -- *
  , Per(..)
  , invertRate
  , invertedRate
  , (|*|)
  , Ticks(..)
  , Tiles(..)
  , TicksPerTile
  , TilesPerTick
  , timesTiles

    -- *
  , Dimensions'(..)
  , Dimensions
  , HasWidth(..)
  , HasHeight(..)

    -- *
  , Direction(..)
  , move
  , asPosition
  , directionOf
  , Cardinal(..)

    -- *
  , Corner(..)
  , Edge(..)
  , cornerEdges

    -- *
  , Neighbors(..)
  , edges
  , neighborDirections
  , neighborPositions
  , arrayNeighbors
  , rotations

    -- *
  , Hitpoints(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (Left, Down, Right, (.=), elements)
--------------------------------------------------------------------------------
import           Linear.V2 hiding (_x, _y)
import qualified Linear.V2 as L
import           Linear.V4 hiding (_x, _y)
import           Test.QuickCheck (Arbitrary, CoArbitrary, Function, elements)
import           Test.QuickCheck.Arbitrary.Generic
import           Data.Group
import           Brick (Location(Location), Edges(..))
import           Data.Monoid (Product(..), Sum(..))
import           Data.Array.IArray
import           Data.Aeson.Generic.DerivingVia
import           Data.Aeson
                 ( ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
--------------------------------------------------------------------------------
import           Xanthous.Util (EqEqProp(..), EqProp, between)
import           Xanthous.Util.QuickCheck (GenericArbitrary(..))
import           Xanthous.Orphans ()
import           Xanthous.Util.Graphics
--------------------------------------------------------------------------------

-- | opposite ∘ opposite ≡ id
class Opposite x where
  opposite :: x -> x

--------------------------------------------------------------------------------

-- fromScalar ∘ scalar ≡ id
class Scalar a where
  scalar :: a -> Double
  fromScalar :: Double -> a

instance Scalar Double where
  scalar = id
  fromScalar = id

newtype ScalarIntegral a = ScalarIntegral a
  deriving newtype (Eq, Ord, Num, Enum, Real, Integral)
instance Integral a => Scalar (ScalarIntegral a) where
  scalar = fromIntegral
  fromScalar = floor

deriving via (ScalarIntegral Integer) instance Scalar Integer
deriving via (ScalarIntegral Word) instance Scalar Word

--------------------------------------------------------------------------------

data Position' a where
  Position :: { _x :: a
             , _y :: a
             } -> (Position' a)
  deriving stock (Show, Eq, Generic, Ord, Functor, Foldable, Traversable)
  deriving anyclass (NFData, Hashable, CoArbitrary, Function)
  deriving EqProp via EqEqProp (Position' a)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
                       (Position' a)

x, y :: Lens' (Position' a) a
x = lens (\(Position xx _) -> xx) (\(Position _ yy) xx -> Position xx yy)
y = lens (\(Position _ yy) -> yy) (\(Position xx _) yy -> Position xx yy)

type Position = Position' Int

instance Arbitrary a => Arbitrary (Position' a) where
  arbitrary = genericArbitrary
  shrink (Position px py) = Position <$> shrink px <*> shrink py


instance Num a => Semigroup (Position' a) where
  (Position x₁ y₁) <> (Position x₂ y₂) = Position (x₁ + x₂) (y₁ + y₂)

instance Num a => Monoid (Position' a) where
  mempty = Position 0 0

instance Num a => Group (Position' a) where
  invert (Position px py) = Position (negate px) (negate py)

-- | Positions convert to scalars by discarding their orientation and just
-- measuring the length from the origin
instance (Ord a, Num a, Scalar a) => Scalar (Position' a) where
  scalar = fromIntegral . length . line (0, 0) . view _Position
  fromScalar n = Position (fromScalar n) (fromScalar n)

data Positioned a where
  Positioned :: Position -> a -> Positioned a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
type role Positioned representational

_Positioned :: Iso (Position, a) (Position, b) (Positioned a) (Positioned b)
_Positioned = iso hither yon
  where
    hither (pos, a) = Positioned pos a
    yon (Positioned pos b) = (pos, b)

instance Arbitrary a => Arbitrary (Positioned a) where
  arbitrary = Positioned <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (Positioned a) where
  toJSON (Positioned pos val) = object
    [ "position" .= pos
    , "data" .= val
    ]

instance FromJSON a => FromJSON (Positioned a) where
  parseJSON = withObject "Positioned" $ \obj ->
    Positioned <$> obj .: "position" <*> obj .: "data"

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

_Position :: Iso' (Position' a) (a, a)
_Position = iso hither yon
  where
    hither (Position px py) = (px, py)
    yon (lx, ly) = Position lx ly

positionFromPair :: (Num a, Integral i, Integral j) => (i, j) -> Position' a
positionFromPair (i, j) = Position (fromIntegral i) (fromIntegral j)

-- | Add two positions
--
-- Operation for the additive group on positions
addPositions :: Num a => Position' a -> Position' a -> Position' a
addPositions = (<>)

-- | Subtract two positions.
--
-- diffPositions pos₁ pos₂ = pos₁ `addPositions` (invert pos₂)
diffPositions :: Num a => Position' a -> Position' a -> Position' a
diffPositions (Position x₁ y₁) (Position x₂ y₂) = Position (x₁ - x₂) (y₁ - y₂)

-- | Is this position a unit position? or: When taken as a difference, does this
-- position represent a step of one tile?
--
-- ∀ dir :: Direction. isUnit ('asPosition' dir)
isUnit :: (Eq a, Num a) => Position' a -> Bool
isUnit (Position px py) =
  abs px `elem` [0,1] && abs py `elem` [0, 1] && (px, py) /= (0, 0)

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
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (CoArbitrary, Function, NFData, ToJSON, FromJSON, Hashable)
  deriving Arbitrary via GenericArbitrary Direction

instance Opposite Direction where
  opposite Up        = Down
  opposite Down      = Up
  opposite Left      = Right
  opposite Right     = Left
  opposite UpLeft    = DownRight
  opposite UpRight   = DownLeft
  opposite DownLeft  = UpRight
  opposite DownRight = UpLeft
  opposite Here      = Here

move :: Num a => Direction -> Position' a -> Position' a
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

-- | Newtype controlling arbitrary generation to only include cardinal
-- directions ('Up', 'Down', 'Left', 'Right')
newtype Cardinal = Cardinal { getCardinal :: Direction }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData, Function, CoArbitrary)
  deriving newtype (Opposite)

instance Arbitrary Cardinal where
  arbitrary = Cardinal <$> elements [Up, Down, Left, Right]

--------------------------------------------------------------------------------

data Corner
  = TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving Arbitrary via GenericArbitrary Corner

instance Opposite Corner where
  opposite TopLeft = BottomRight
  opposite TopRight = BottomLeft
  opposite BottomLeft = TopRight
  opposite BottomRight = TopLeft

data Edge
  = TopEdge
  | LeftEdge
  | RightEdge
  | BottomEdge
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving Arbitrary via GenericArbitrary Edge

instance Opposite Edge where
  opposite TopEdge = BottomEdge
  opposite BottomEdge = TopEdge
  opposite LeftEdge = RightEdge
  opposite RightEdge = LeftEdge

cornerEdges :: Corner -> (Edge, Edge)
cornerEdges TopLeft = (TopEdge, LeftEdge)
cornerEdges TopRight = (TopEdge, RightEdge)
cornerEdges BottomLeft = (BottomEdge, LeftEdge)
cornerEdges BottomRight = (BottomEdge, RightEdge)

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
  deriving anyclass (NFData, CoArbitrary, Function, MonoFoldable)
  deriving Arbitrary via GenericArbitrary (Neighbors a)

type instance Element (Neighbors a) = a

makeFieldsNoPrefix ''Neighbors

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

neighborPositions :: Num a => Position' a -> Neighbors (Position' a)
neighborPositions pos = (`move` pos) <$> neighborDirections

arrayNeighbors
  :: (IArray a e, Ix i, Num i)
  => a (i, i) e
  -> (i, i)
  -> Neighbors (Maybe e)
arrayNeighbors arr center = arrLookup <$> neighborPositions (_Position # center)
  where
    arrLookup (view _Position -> pos)
      | inRange (bounds arr) pos = Just $ arr ! pos
      | otherwise                = Nothing

-- | Returns a list of all 4 90-degree rotations of the given neighbors
rotations :: Neighbors a -> V4 (Neighbors a)
rotations orig@(Neighbors tl t tr l r bl b br) = V4
   orig                            -- tl t  tr
                                   -- l     r
                                   -- bl b  br

   (Neighbors bl l tl b t br r tr) -- bl l tl
                                   -- b    t
                                   -- br r tr

   (Neighbors br b bl r l tr t tl) -- br b bl
                                   -- r    l
                                   -- tr t tl

   (Neighbors tr r br t b tl l bl) -- tr r br
                                   -- t    b
                                   -- tl l bl

--------------------------------------------------------------------------------

newtype Per a b = Rate Double
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (Num, Ord, Enum, Real, Fractional, ToJSON, FromJSON) via Double
  deriving (Semigroup, Monoid) via Product Double
instance Arbitrary (Per a b) where arbitrary = genericArbitrary

invertRate :: a `Per` b -> b `Per` a
invertRate (Rate p) = Rate $ 1 / p

invertedRate :: Iso (a `Per` b) (b' `Per` a') (b `Per` a) (a' `Per` b')
invertedRate = iso invertRate invertRate

infixl 7 |*|
(|*|) :: (Scalar a, Scalar b) => a `Per` b -> b -> a
(|*|) (Rate rate) b = fromScalar $ rate * scalar b

newtype Ticks = Ticks Word
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (Num, Ord, Bounded, Enum, Integral, Real, ToJSON, FromJSON) via Word
  deriving (Semigroup, Monoid) via (Sum Word)
  deriving Scalar via ScalarIntegral Ticks
instance Arbitrary Ticks where arbitrary = genericArbitrary

newtype Tiles = Tiles Double
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (Num, Ord, Enum, Real, ToJSON, FromJSON, Scalar) via Double
  deriving (Semigroup, Monoid) via (Sum Double)
instance Arbitrary Tiles where arbitrary = genericArbitrary

type TicksPerTile = Ticks `Per` Tiles
type TilesPerTick = Tiles `Per` Ticks

timesTiles :: TicksPerTile -> Tiles -> Ticks
timesTiles = (|*|)

--------------------------------------------------------------------------------

newtype Hitpoints = Hitpoints Word
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (Arbitrary, Num, Ord, Bounded, Enum, Integral, Real, ToJSON, FromJSON)
       via Word
  deriving (Semigroup, Monoid) via Sum Word

--------------------------------------------------------------------------------

data Box a = Box
  { _topLeftCorner :: V2 a
  , _dimensions    :: V2 a
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving Arbitrary via GenericArbitrary (Box a)
makeFieldsNoPrefix ''Box

bottomRightCorner :: Num a => Box a -> V2 a
bottomRightCorner box =
  V2 (box ^. topLeftCorner . L._x + box ^. dimensions . L._x)
     (box ^. topLeftCorner . L._y + box ^. dimensions . L._y)

setBottomRightCorner :: (Num a, Ord a) => Box a -> V2 a -> Box a
setBottomRightCorner box br@(V2 brx bry)
  | brx < box ^. topLeftCorner . L._x || bry < box ^. topLeftCorner . L._y
  = box & topLeftCorner .~ br
        & dimensions . L._x .~ ((box ^. topLeftCorner . L._x) - brx)
        & dimensions . L._y .~ ((box ^. topLeftCorner . L._y) - bry)
  | otherwise
  = box & dimensions . L._x .~ (brx - (box ^. topLeftCorner . L._x))
        & dimensions . L._y .~ (bry - (box ^. topLeftCorner . L._y))

inBox :: (Ord a, Num a) => Box a -> V2 a -> Bool
inBox box pt = flip all [L._x, L._y] $ \component ->
  between (box ^. topLeftCorner . component)
          (box ^. to bottomRightCorner . component)
          (pt ^. component)

boxIntersects :: (Ord a, Num a) => Box a -> Box a -> Bool
boxIntersects box₁ box₂
  = any (inBox box₁) [box₂ ^. topLeftCorner, bottomRightCorner box₂]

boxCenter :: (Fractional a) => Box a -> V2 a
boxCenter box = V2 cx cy
 where
   cx = box ^. topLeftCorner . L._x + (box ^. dimensions . L._x / 2)
   cy = box ^. topLeftCorner . L._y + (box ^. dimensions . L._y / 2)

boxEdge :: (Enum a, Num a) => Box a -> Edge -> [V2 a]
boxEdge box LeftEdge =
  V2 (box ^. topLeftCorner . L._x)
  <$> [box ^. topLeftCorner . L._y .. box ^. to bottomRightCorner . L._y]
boxEdge box RightEdge =
  V2 (box ^. to bottomRightCorner . L._x)
  <$> [box ^. to bottomRightCorner . L._y .. box ^. to bottomRightCorner . L._y]
boxEdge box TopEdge =
  flip V2 (box ^. topLeftCorner . L._y)
  <$> [box ^. topLeftCorner . L._x .. box ^. to bottomRightCorner . L._x]
boxEdge box BottomEdge =
  flip V2 (box ^. to bottomRightCorner . L._y)
  <$> [box ^. topLeftCorner . L._x .. box ^. to bottomRightCorner . L._x]

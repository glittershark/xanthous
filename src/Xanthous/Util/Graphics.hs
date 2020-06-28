{-# LANGUAGE TemplateHaskell #-}
-- | Graphics algorithms and utils for rendering things in 2D space
--------------------------------------------------------------------------------
module Xanthous.Util.Graphics
  ( circle
  , filledCircle
  , line
  , straightLine
  , delaunay

    -- * Debugging and testing tools
  , renderBooleanGraphics
  , showBooleanGraphics
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
-- https://github.com/noinia/hgeometry/issues/28
-- import qualified Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
--               as Geometry
import qualified Algorithms.Geometry.DelaunayTriangulation.Naive
              as Geometry
import qualified Algorithms.Geometry.DelaunayTriangulation.Types as Geometry
import           Control.Monad.State (execState, State)
import qualified Data.Geometry.Point as Geometry
import           Data.Ext ((:+)(..))
import           Data.List (unfoldr)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Ix (Ix)
import           Linear.V2
--------------------------------------------------------------------------------


-- | Generate a circle centered at the given point and with the given radius
-- using the <midpoint circle algorithm
-- https://en.wikipedia.org/wiki/Midpoint_circle_algorithm>.
--
-- Code taken from <https://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#Haskell>
circle :: (Num i, Ord i)
       => (i, i) -- ^ center
       -> i      -- ^ radius
       -> [(i, i)]
circle (x₀, y₀) radius
  -- Four initial points, plus the generated points
  = (x₀, y₀ + radius) : (x₀, y₀ - radius) : (x₀ + radius, y₀) : (x₀ - radius, y₀) : points
    where
      -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
      points = concatMap generatePoints $ unfoldr step initialValues

      generatePoints (x, y)
        = [ (x₀ `xop` x', y₀ `yop` y')
          | (x', y') <- [(x, y), (y, x)]
          , xop <- [(+), (-)]
          , yop <- [(+), (-)]
          ]

      initialValues = (1 - radius, 1, (-2) * radius, 0, radius)

      step (f, ddf_x, ddf_y, x, y)
        | x >= y = Nothing
        | otherwise = Just ((x', y'), (f', ddf_x', ddf_y', x', y'))
        where
          (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                           | otherwise = (f + ddf_x, ddf_y, y)
          ddf_x' = ddf_x + 2
          x' = x + 1


data FillState i
  = FillState
  { _inCircle :: Bool
  , _result :: NonEmpty (i, i)
  }
makeLenses ''FillState

runFillState :: NonEmpty (i, i) -> State (FillState i) a -> [(i, i)]
runFillState circumference s
  = toList
  . view result
  . execState s
  $ FillState False circumference

-- | Generate a *filled* circle centered at the given point and with the given
-- radius by filling a circle generated with 'circle'
filledCircle :: (Num i, Integral i, Ix i)
             => (i, i) -- ^ center
             -> i      -- ^ radius
             -> [(i, i)]
filledCircle origin radius =
  case NE.nonEmpty (circle origin radius) of
    Nothing -> []
    Just circumference -> runFillState circumference $
      -- the first and last lines of all circles are solid, so the whole "in the
      -- circle, out of the circle" thing doesn't work... but that's fine since
      -- we don't need to fill them. So just skip them
      for_ [succ minX..pred maxX] $ \x ->
        for_ [minY..maxY] $ \y -> do
          let pt = (x, y)
              next = (x, succ y)
          whenM (use inCircle) $ result %= NE.cons pt

          when (pt `elem` circumference && next `notElem` circumference)
            $ inCircle %= not

      where
        ((minX, minY), (maxX, maxY)) = minmaxes circumference

-- | Draw a line between two points using Bresenham's line drawing algorithm
--
-- Code taken from <https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm>
line :: (Num i, Ord i) => (i, i) -> (i, i) -> [(i, i)]
line pa@(xa, ya) pb@(xb, yb)
  = (if maySwitch pa < maySwitch pb then id else reverse) points
  where
    points               = map maySwitch . unfoldr go $ (x₁, y₁, 0)
    steep                = abs (yb - ya) > abs (xb - xa)
    maySwitch            = if steep then swap else id
    [(x₁, y₁), (x₂, y₂)] = sort [maySwitch pa, maySwitch pb]
    δx                   = x₂ - x₁
    δy                   = abs (y₂ - y₁)
    ystep                = if y₁ < y₂ then 1 else -1
    go (xTemp, yTemp, err)
      | xTemp > x₂ = Nothing
      | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newError))
      where
        tempError        = err + δy
        (newY, newError) = if (2 * tempError) >= δx
                           then (yTemp + ystep, tempError - δx)
                           else (yTemp, tempError)
{-# SPECIALIZE line :: (Int, Int) -> (Int, Int) -> [(Int, Int)] #-}
{-# SPECIALIZE line :: (Word, Word) -> (Word, Word) -> [(Word, Word)] #-}

straightLine :: (Num i, Ord i) => (i, i) -> (i, i) -> [(i, i)]
straightLine pa@(xa, _) pb@(_, yb) = line pa midpoint ++ line midpoint pb
  where midpoint = (xa, yb)


delaunay
  :: (Ord n, Fractional n)
  => NonEmpty (V2 n, p)
  -> [((V2 n, p), (V2 n, p))]
delaunay
  = map (over both fromPoint)
  . Geometry.triangulationEdges
  . Geometry.delaunayTriangulation
  . map toPoint
  where
    toPoint (V2 px py, pid) = Geometry.Point2 px py :+ pid
    fromPoint (Geometry.Point2 px py :+ pid) = (V2 px py, pid)

--------------------------------------------------------------------------------

renderBooleanGraphics :: forall i. (Num i, Ord i, Enum i) => [(i, i)] -> String
renderBooleanGraphics [] = ""
renderBooleanGraphics (pt : pts') = intercalate "\n" rows
  where
    rows = row <$> [minX..maxX]
    row x = [minY..maxY] <&> \y -> if (x, y) `member` ptSet then 'X' else ' '
    ((minX, minY), (maxX, maxY)) = minmaxes pts
    pts = pt :| pts'
    ptSet :: Set (i, i)
    ptSet = setFromList $ toList pts

showBooleanGraphics :: forall i. (Num i, Ord i, Enum i) => [(i, i)] -> IO ()
showBooleanGraphics = putStrLn . pack . renderBooleanGraphics

minmaxes :: forall i. (Ord i) => NonEmpty (i, i) -> ((i, i), (i, i))
minmaxes xs =
    ( ( minimum1Of (traverse1 . _1) xs
      , minimum1Of (traverse1 . _2) xs
      )
    , ( maximum1Of (traverse1 . _1) xs
      , maximum1Of (traverse1 . _2) xs
      )
    )

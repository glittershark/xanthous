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
       => V2 i -- ^ center
       -> i    -- ^ radius
       -> [V2 i]
circle (V2 x₀ y₀) radius
  -- Four initial points, plus the generated points
  = V2 x₀ (y₀ + radius)
  : V2 x₀ (y₀ - radius)
  : V2 (x₀ + radius) y₀
  : V2 (x₀ - radius) y₀
  : points
    where
      -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
      points = concatMap generatePoints $ unfoldr step initialValues

      generatePoints (V2 x y)
        = [ V2 (x₀ `xop` x') (y₀ `yop` y')
          | (x', y') <- [(x, y), (y, x)]
          , xop <- [(+), (-)]
          , yop <- [(+), (-)]
          ]

      initialValues = (1 - radius, 1, (-2) * radius, 0, radius)

      step (f, ddf_x, ddf_y, x, y)
        | x >= y = Nothing
        | otherwise = Just (V2 x' y', (f', ddf_x', ddf_y', x', y'))
        where
          (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                           | otherwise = (f + ddf_x, ddf_y, y)
          ddf_x' = ddf_x + 2
          x' = x + 1


data FillState i
  = FillState
  { _inCircle :: Bool
  , _result :: NonEmpty (V2 i)
  }
makeLenses ''FillState

runFillState :: NonEmpty (V2 i) -> State (FillState i) a -> [V2 i]
runFillState circumference s
  = toList
  . view result
  . execState s
  $ FillState False circumference

-- | Generate a *filled* circle centered at the given point and with the given
-- radius by filling a circle generated with 'circle'
filledCircle :: (Num i, Integral i, Ix i)
             => V2 i -- ^ center
             -> i    -- ^ radius
             -> [V2 i]
filledCircle center radius =
  case NE.nonEmpty (circle center radius) of
    Nothing -> []
    Just circumference -> runFillState circumference $
      -- the first and last lines of all circles are solid, so the whole "in the
      -- circle, out of the circle" thing doesn't work... but that's fine since
      -- we don't need to fill them. So just skip them
      for_ [succ minX..pred maxX] $ \x ->
        for_ [minY..maxY] $ \y -> do
          let pt = V2 x y
              next = V2 x $ succ y
          whenM (use inCircle) $ result %= NE.cons pt

          when (pt `elem` circumference && next `notElem` circumference)
            $ inCircle %= not

      where
        (V2 minX minY, V2 maxX maxY) = minmaxes circumference

-- | Draw a line between two points using Bresenham's line drawing algorithm
--
-- Code taken from <https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm>
line :: (Num i, Ord i) => V2 i -> V2 i -> [V2 i]
line pa@(V2 xa ya) pb@(V2 xb yb)
  = (if maySwitch pa < maySwitch pb then id else reverse) points
  where
    points               = map maySwitch . unfoldr go $ (x₁, y₁, 0)
    steep                = abs (yb - ya) > abs (xb - xa)
    maySwitch            = if steep then view _yx else id
    [V2 x₁ y₁, V2 x₂ y₂] = sort [maySwitch pa, maySwitch pb]
    δx                   = x₂ - x₁
    δy                   = abs (y₂ - y₁)
    ystep                = if y₁ < y₂ then 1 else -1
    go (xTemp, yTemp, err)
      | xTemp > x₂ = Nothing
      | otherwise  = Just ((V2 xTemp yTemp), (xTemp + 1, newY, newError))
      where
        tempError        = err + δy
        (newY, newError) = if (2 * tempError) >= δx
                           then (yTemp + ystep, tempError - δx)
                           else (yTemp, tempError)
{-# SPECIALIZE line :: V2 Int -> V2 Int -> [V2 Int] #-}
{-# SPECIALIZE line :: V2 Word -> V2 Word -> [V2 Word] #-}

straightLine :: (Num i, Ord i) => V2 i -> V2 i -> [V2 i]
straightLine pa@(V2 xa _) pb@(V2 _ yb) = line pa midpoint ++ line midpoint pb
  where midpoint = V2 xa yb


delaunay
  :: (Ord n, Fractional n)
  => NonEmpty (V2 n, p)
  -> [((V2 n, p), (V2 n, p))]
delaunay
  = map (over both fromPoint)
  . Geometry.edgesAsPoints
  . Geometry.delaunayTriangulation
  . map toPoint
  where
    toPoint (V2 px py, pid) = Geometry.Point2 px py :+ pid
    fromPoint (Geometry.Point2 px py :+ pid) = (V2 px py, pid)

--------------------------------------------------------------------------------

renderBooleanGraphics :: forall i. (Num i, Ord i, Enum i) => [V2 i] -> String
renderBooleanGraphics [] = ""
renderBooleanGraphics (pt : pts') = intercalate "\n" rows
  where
    rows = row <$> [minX..maxX]
    row x = [minY..maxY] <&> \y -> if V2 x y `member` ptSet then 'X' else ' '
    (V2 minX minY, V2 maxX maxY) = minmaxes pts
    pts = pt :| pts'
    ptSet :: Set (V2 i)
    ptSet = setFromList $ toList pts

showBooleanGraphics :: forall i. (Num i, Ord i, Enum i) => [V2 i] -> IO ()
showBooleanGraphics = putStrLn . pack . renderBooleanGraphics

minmaxes :: forall i. (Ord i) => NonEmpty (V2 i) -> (V2 i, V2 i)
minmaxes xs =
  ( V2 (minimum1Of (traverse1 . _x) xs)
       (minimum1Of (traverse1 . _y) xs)
  , V2 (maximum1Of (traverse1 . _x) xs)
       (maximum1Of (traverse1 . _y) xs)
  )

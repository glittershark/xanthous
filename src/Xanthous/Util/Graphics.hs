-- | Graphics algorithms and utils for rendering things in 2D space
--------------------------------------------------------------------------------
module Xanthous.Util.Graphics
  ( circle
  , filledCircle
  , line
  , straightLine
  , delaunay
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import qualified Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
              as Geometry
import qualified Algorithms.Geometry.DelaunayTriangulation.Types as Geometry
import           Codec.Picture (imagePixels)
import qualified Data.Geometry.Point as Geometry
import           Data.Ext ((:+)(..))
import           Data.List (unfoldr)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Ix (range, Ix)
import           Data.Word (Word8)
import qualified Graphics.Rasterific as Raster
import           Graphics.Rasterific hiding (circle, line, V2(..))
import           Graphics.Rasterific.Texture (uniformTexture)
import           Linear.V2
--------------------------------------------------------------------------------


circle :: (Num i, Integral i, Ix i)
       => (i, i) -- ^ center
       -> i      -- ^ radius
       -> [(i, i)]
circle (ox, oy) radius
  = pointsFromRaster (ox + radius) (oy + radius)
  $ stroke 1 JoinRound (CapRound, CapRound)
  $ Raster.circle (Raster.V2 (fromIntegral ox) (fromIntegral oy))
  $ fromIntegral radius

filledCircle :: (Num i, Integral i, Ix i)
             => (i, i) -- ^ center
             -> i      -- ^ radius
             -> [(i, i)]
filledCircle (ox, oy) radius
  = pointsFromRaster (ox + radius) (oy + radius)
  $ fill
  $ Raster.circle (Raster.V2 (fromIntegral ox) (fromIntegral oy))
  $ fromIntegral radius

-- showCells . fromPoints . NE.fromList $ filledCircle (15, 15) 7
-- pointsFromRaster :: (Num i, Integral i, Ix i)
--                  => i      -- ^ width
--                  -> i      -- ^ height
--                  -> _
--                  -> [(i, i)]
pointsFromRaster
  :: (Integral a, Integral b, Ix a, Ix b)
  => a
  -> b
  -> Drawing Word8 ()
  -> [(a, b)]
pointsFromRaster w h raster
  = map snd
  $ filter ((== 1) . fst)
  $ zip pixels
  $ range ((1, 1), (w, h))
  where
    pixels = toListOf imagePixels
           $ renderDrawing @Word8 (fromIntegral w) (fromIntegral h) 0
           $ withTexture (uniformTexture 1) raster



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

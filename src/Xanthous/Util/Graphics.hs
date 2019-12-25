-- | Graphics algorithms and utils for rendering things in 2D space
--------------------------------------------------------------------------------
module Xanthous.Util.Graphics
  ( circle
  , filledCircle
  , line
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Data.List (unfoldr)
import           Data.Ix (range, Ix)
import           Data.Word (Word8)
import qualified Graphics.Rasterific as Raster
import           Graphics.Rasterific hiding (circle, line)
import           Graphics.Rasterific.Texture (uniformTexture)
import           Codec.Picture (imagePixels)
--------------------------------------------------------------------------------


circle :: (Num i, Integral i, Ix i)
       => (i, i) -- ^ center
       -> i      -- ^ radius
       -> [(i, i)]
circle (ox, oy) radius
  = pointsFromRaster (ox + radius) (oy + radius)
  $ stroke 1 JoinRound (CapRound, CapRound)
  $ Raster.circle (V2 (fromIntegral ox) (fromIntegral oy))
  $ fromIntegral radius

filledCircle :: (Num i, Integral i, Ix i)
             => (i, i) -- ^ center
             -> i      -- ^ radius
             -> [(i, i)]
filledCircle (ox, oy) radius
  = pointsFromRaster (ox + radius) (oy + radius)
  $ fill
  $ Raster.circle (V2 (fromIntegral ox) (fromIntegral oy))
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

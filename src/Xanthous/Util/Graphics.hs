-- | Graphics algorithms and utils for rendering things in 2D space
--------------------------------------------------------------------------------
module Xanthous.Util.Graphics where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Data.List (unfoldr)
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

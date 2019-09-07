-- |

module Xanthous.Generators.Util
  ( Cells
  , CellM
  , randInitialize
  , numAliveNeighborsM
  , cloneMArray
  ) where

import Xanthous.Prelude
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST
import Control.Monad.Random
import Data.Monoid

import Xanthous.Util (foldlMapM')
import Xanthous.Data (Dimensions, width, height)

type Cells s = STUArray s (Word, Word) Bool
type CellM g s a = RandT g (ST s) a

randInitialize :: RandomGen g => Dimensions -> Double -> CellM g s (Cells s)
randInitialize dims aliveChance = do
  res <- lift $ newArray ((0, 0), (dims ^. width, dims ^. height)) False
  for_ [0..dims ^. width] $ \i ->
    for_ [0..dims ^. height] $ \j -> do
      val <- (>= aliveChance) <$> getRandomR (0, 1)
      lift $ writeArray res (i, j) val
  pure res

numAliveNeighborsM
  :: forall a i j m
  . (MArray a Bool m, Ix (i, j), Integral i, Integral j)
  => a (i, j) Bool
  -> (i, j)
  -> m Word
numAliveNeighborsM cells (x, y) = do
  cellBounds <- getBounds cells
  getSum <$> foldlMapM'
    (fmap (Sum . fromIntegral . fromEnum) . boundedGet cellBounds)
    neighborPositions

  where
    boundedGet :: ((i, j), (i, j)) -> (Int, Int) -> m Bool
    boundedGet ((minX, minY), (maxX, maxY)) (i, j)
      | x <= minX
        || y <= minY
        || x >= maxX
        || y >= maxY
      = pure True
      | otherwise =
        let nx = fromIntegral $ fromIntegral x + i
            ny = fromIntegral $ fromIntegral y + j
        in readArray cells (nx, ny)

    neighborPositions :: [(Int, Int)]
    neighborPositions = [(i, j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0, 0)]

cloneMArray
  :: forall a a' i e m.
  ( Ix i
  , MArray a e m
  , MArray a' e m
  , IArray UArray e
  )
  => a i e
  -> m (a' i e)
cloneMArray = thaw @_ @UArray <=< freeze

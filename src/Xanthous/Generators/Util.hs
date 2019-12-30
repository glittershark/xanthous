{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Util
  ( MCells
  , Cells
  , CellM
  , randInitialize
  , initializeEmpty
  , numAliveNeighborsM
  , numAliveNeighbors
  , fillOuterEdgesM
  , cloneMArray
  , floodFill
  , regions
  , fillAll
  , fillAllM
  , fromPoints
  , fromPointsM
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (Foldable, toList, for_)
import           Data.Array.ST
import           Data.Array.Unboxed
import           Control.Monad.ST
import           Control.Monad.Random
import           Data.Monoid
import           Data.Foldable (Foldable, toList, for_)
import qualified Data.Set as Set
import           Data.Semigroup.Foldable
--------------------------------------------------------------------------------
import           Xanthous.Util (foldlMapM', maximum1, minimum1)
import           Xanthous.Data (Dimensions, width, height)
--------------------------------------------------------------------------------

type MCells s = STUArray s (Word, Word) Bool
type Cells = UArray (Word, Word) Bool
type CellM g s a = RandT g (ST s) a

randInitialize :: RandomGen g => Dimensions -> Double -> CellM g s (MCells s)
randInitialize dims aliveChance = do
  res <- initializeEmpty dims
  for_ [0..dims ^. width] $ \i ->
    for_ [0..dims ^. height] $ \j -> do
      val <- (>= aliveChance) <$> getRandomR (0, 1)
      lift $ writeArray res (i, j) val
  pure res

initializeEmpty :: RandomGen g => Dimensions -> CellM g s (MCells s)
initializeEmpty dims =
  lift $ newArray ((0, 0), (dims ^. width, dims ^. height)) False

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

numAliveNeighbors
  :: forall a i j
  . (IArray a Bool, Ix (i, j), Integral i, Integral j)
  => a (i, j) Bool
  -> (i, j)
  -> Word
numAliveNeighbors cells (x, y) =
  let cellBounds = bounds cells
  in getSum $ foldMap
      (Sum . fromIntegral . fromEnum . boundedGet cellBounds)
      neighborPositions

  where
    boundedGet :: ((i, j), (i, j)) -> (Int, Int) -> Bool
    boundedGet ((minX, minY), (maxX, maxY)) (i, j)
      | x <= minX
        || y <= minY
        || x >= maxX
        || y >= maxY
      = True
      | otherwise =
        let nx = fromIntegral $ fromIntegral x + i
            ny = fromIntegral $ fromIntegral y + j
        in cells ! (nx, ny)

    neighborPositions :: [(Int, Int)]
    neighborPositions = [(i, j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0, 0)]

fillOuterEdgesM :: (MArray a Bool m, Ix i, Ix j) => a (i, j) Bool -> m ()
fillOuterEdgesM arr = do
  ((minX, minY), (maxX, maxY)) <- getBounds arr
  for_ (range (minX, maxX)) $ \x -> do
    writeArray arr (x, minY) True
    writeArray arr (x, maxY) True
  for_ (range (minY, maxY)) $ \y -> do
    writeArray arr (minX, y) True
    writeArray arr (maxX, y) True

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

--------------------------------------------------------------------------------

-- | Flood fill a cell array starting at a point, returning a list of all the
-- (true) cell locations reachable from that point
floodFill :: forall a i j.
            ( IArray a Bool
            , Ix (i, j)
            , Enum i , Enum j
            , Bounded i , Bounded j
            , Eq i , Eq j
            , Show i, Show j
            )
          => a (i, j) Bool -- ^ array
          -> (i, j)        -- ^ position
          -> Set (i, j)
floodFill = go mempty
  where
    go :: Set (i, j) -> a (i, j) Bool -> (i, j) -> Set (i, j)
    -- TODO pass result in rather than passing seen in, return result
    go res arr@(bounds -> arrBounds) idx@(x, y)
      | not (inRange arrBounds idx) =  res
      | not (arr ! idx) =  res
      | otherwise =
        let neighbors
              = filter (inRange arrBounds)
              . filter (/= idx)
              . filter (`notMember` res)
              $ (,)
              <$> [(if x == minBound then x else pred x)
                   ..
                   (if x == maxBound then x else succ x)]
              <*> [(if y == minBound then y else pred y)
                   ..
                   (if y == maxBound then y else succ y)]
        in foldl' (\r idx' ->
                     if arr ! idx'
                     then r <> go (r & contains idx' .~ True) arr idx'
                     else r)
           (res & contains idx .~ True) neighbors

-- | Gives a list of all the disconnected regions in a cell array, represented
-- each as lists of points
regions :: forall a i j.
          ( IArray a Bool
          , Ix (i, j)
          , Enum i , Enum j
          , Bounded i , Bounded j
          , Eq i , Eq j
          , Show i, Show j
          )
        => a (i, j) Bool
        -> [Set (i, j)]
regions arr
  | Just firstPoint <- findFirstPoint arr =
      let region = floodFill arr firstPoint
          arr' = fillAll region arr
      in region : regions arr'
  | otherwise = []
  where
    findFirstPoint :: a (i, j) Bool -> Maybe (i, j)
    findFirstPoint = fmap fst . headMay . filter snd . assocs

fillAll :: (IArray a Bool, Ix i, Foldable f) => f i -> a i Bool -> a i Bool
fillAll ixes a = accum (const fst) a $ (, (False, ())) <$> toList ixes

fillAllM :: (MArray a Bool m, Ix i, Foldable f) => f i -> a i Bool -> m ()
fillAllM ixes a = for_ ixes $ \i -> writeArray a i False

fromPoints
  :: forall a f i.
    ( IArray a Bool
    , Ix i
    , Functor f
    , Foldable1 f
    )
  => f (i, i)
  -> a (i, i) Bool
fromPoints points =
  let pts = Set.fromList $ toList points
      dims = ( (minimum1 $ fst <$> points, minimum1 $ snd <$> points)
             , (maximum1 $ fst <$> points, maximum1 $ snd <$> points)
             )
  in array dims $ range dims <&> \i -> (i, i `member` pts)

fromPointsM
  :: (MArray a Bool m, Ix i, Element f ~ i, MonoFoldable f)
  => NonNull f
  -> m (a i Bool)
fromPointsM points = do
  arr <- newArray (minimum points, maximum points) False
  fillAllM (otoList points) arr
  pure arr

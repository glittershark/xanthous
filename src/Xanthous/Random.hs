--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.Random
  ( Choose(..)
  , ChooseElement(..)
  , Weighted(..)
  , evenlyWeighted
  , weightedBy
  , subRand
  , chance
  , chooseSubset
  , chooseRange
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty(..))
import           Control.Monad.Random.Class (MonadRandom(getRandomR, getRandom))
import           Control.Monad.Random (Rand, evalRand, mkStdGen, StdGen)
import           Data.Functor.Compose
import           Data.Random.Shuffle.Weighted
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform
import           Data.Random.Distribution.Uniform.Exclusive
import           Data.Random.Sample
import qualified Data.Random.Source as DRS
import           Data.Interval ( Interval, lowerBound', Extended (Finite)
                               , upperBound', Boundary (Closed)
                               )
--------------------------------------------------------------------------------

instance {-# INCOHERENT #-} (Monad m, MonadRandom m) => DRS.MonadRandom m where
  getRandomWord8 = getRandom
  getRandomWord16 = getRandom
  getRandomWord32 = getRandom
  getRandomWord64 = getRandom
  getRandomDouble = getRandom
  getRandomNByteInteger n = getRandomR (0, 256 ^ n)

class Choose a where
  type RandomResult a
  choose :: MonadRandom m => a -> m (RandomResult a)

newtype ChooseElement a = ChooseElement a

instance MonoFoldable a => Choose (ChooseElement a) where
  type RandomResult (ChooseElement a) = Maybe (Element a)
  choose (ChooseElement xs) = do
    chosenIdx <- getRandomR (0, olength xs - 1)
    let pick _ (Just x) = Just x
        pick (x, i) Nothing
          | i == chosenIdx = Just x
          | otherwise = Nothing
    pure $ ofoldr pick Nothing $ zip (toList xs) [0..]

instance MonoFoldable a => Choose (NonNull a) where
  type RandomResult (NonNull a) = Element a
  choose
    = fmap (fromMaybe (error "unreachable")) -- why not lol
    . choose
    . ChooseElement
    . toNullable

instance Choose (NonEmpty a) where
  type RandomResult (NonEmpty a) = a
  choose = choose . fromNonEmpty @[_]

instance Choose (a, a) where
  type RandomResult (a, a) = a
  choose (x, y) = choose (x :| [y])

newtype Weighted w t a = Weighted (t (w, a))
  deriving (Functor, Foldable) via (t `Compose` (,) w)

instance Traversable t => Traversable (Weighted w t) where
  traverse f (Weighted twa) = Weighted <$> (traverse . traverse) f twa

evenlyWeighted :: [a] -> Weighted Int [] a
evenlyWeighted = Weighted . itoList

-- | Weight the elements of some functor by a function. Larger values of 'w' per
-- its 'Ord' instance will be more likely to be generated
weightedBy :: Functor t => (a -> w) -> t a -> Weighted w t a
weightedBy weighting xs = Weighted $ (weighting &&& id) <$> xs

instance (Num w, Ord w, Distribution Uniform w, Excludable w)
       => Choose (Weighted w [] a) where
  type RandomResult (Weighted w [] a) = Maybe a
  choose (Weighted ws) = sample $ headMay <$> weightedSample 1 ws

instance (Num w, Ord w, Distribution Uniform w, Excludable w)
       => Choose (Weighted w NonEmpty a) where
  type RandomResult (Weighted w NonEmpty a) = a
  choose (Weighted ws) =
    sample
    $ fromMaybe (error "unreachable") . headMay
    <$> weightedSample 1 (toList ws)

subRand :: MonadRandom m => Rand StdGen a -> m a
subRand sub = evalRand sub . mkStdGen <$> getRandom

-- | Has a @n@ chance of returning 'True'
--
-- eg, chance 0.5 will return 'True' half the time
chance
  :: (Num w, Ord w, Distribution Uniform w, Excludable w, MonadRandom m)
  => w
  -> m Bool
chance n = choose $ weightedBy (bool 1 (n * 2)) bools

-- | Choose a random subset of *about* @w@ of the elements of the given
-- 'Witherable' structure
chooseSubset :: ( Num w, Ord w, Distribution Uniform w, Excludable w
               , Witherable t
               , MonadRandom m
               ) => w -> t a -> m (t a)
chooseSubset = filterA . const . chance

-- | Choose a random @n@ in the given interval
chooseRange
  :: ( MonadRandom m
    , Distribution Uniform n
    , Enum n
    , Bounded n, Show n, Ord n)
  => Interval n
  -> m (Maybe n)
chooseRange int = traverse sample distribution
  where
    (lower, lowerBoundary) = lowerBound' int
    lowerR = case lower of
      Finite x -> if lowerBoundary == Closed
                 then x
                 else succ x
      _ -> minBound
    (upper, upperBoundary) = upperBound' int
    upperR = case upper of
      Finite x -> if upperBoundary == Closed
                 then x
                 else pred x
      _ -> maxBound
    distribution
      | lowerR <= upperR = Just $ Uniform lowerR upperR
      | otherwise = Nothing


--------------------------------------------------------------------------------

bools :: NonEmpty Bool
bools = True :| [False]

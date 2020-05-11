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
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty(..))
import           Control.Monad.Random.Class (MonadRandom(getRandomR, getRandom))
import           Control.Monad.Random (Rand, evalRand, mkStdGen, StdGen)
import           Data.Random.Shuffle.Weighted
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform
import           Data.Random.Distribution.Uniform.Exclusive
import           Data.Random.Sample
import qualified Data.Random.Source as DRS
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

evenlyWeighted :: [a] -> Weighted Int [] a
evenlyWeighted = Weighted . itoList

weightedBy :: Functor t => (a -> w) -> t a -> Weighted w t a
weightedBy weighting xs = Weighted $ (weighting &&& id) <$> xs

instance (Num w, Ord w, Distribution Uniform w, Excludable w) => Choose (Weighted w [] a) where
  type RandomResult (Weighted w [] a) = Maybe a
  choose (Weighted ws) = sample $ headMay <$> weightedSample 1 ws

instance (Num w, Ord w, Distribution Uniform w, Excludable w) => Choose (Weighted w NonEmpty a) where
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

--------------------------------------------------------------------------------

bools :: NonEmpty Bool
bools = True :| [False]

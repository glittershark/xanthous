{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanthous.Random
  ( Choose(..)
  , ChooseElement(..)
  ) where

import Xanthous.Prelude
import Data.List.NonEmpty (NonEmpty)
import System.Random
import Control.Monad.Random.Class (MonadRandom(getRandomR))

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

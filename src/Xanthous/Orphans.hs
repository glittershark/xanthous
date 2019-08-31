{-# LANGUAGE UndecidableInstances, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Xanthous.Orphans () where

import Xanthous.Prelude

instance forall s a.
  ( Cons s s a a
  , MonoFoldable s
  ) => Cons (NonNull s) (NonNull s) a a where
  _Cons = prism hither yon
    where
      hither :: (a, NonNull s) -> NonNull s
      hither (a, ns) =
        let s = toNullable ns
        in impureNonNull $ a <| s

      yon :: NonNull s -> Either (NonNull s) (a, NonNull s)
      yon ns = case ns ^? _Cons of
        Nothing -> Left ns
        Just (a, ns') -> Right (a, ns')

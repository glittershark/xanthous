
module Xanthous.Util.Inflection
  ( toSentence
  ) where

import Xanthous.Prelude

toSentence :: (MonoFoldable mono, Element mono ~ Text) => mono -> Text
toSentence xs = case reverse . toList $ xs of
  [] -> ""
  [x] -> x
  [b, a] -> a <> " and " <> b
  (final : butlast) ->
    intercalate ", " (reverse butlast) <> ", and " <> final

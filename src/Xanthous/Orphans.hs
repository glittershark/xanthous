{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances, PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.Orphans
  ( ppTemplate
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (elements)
--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text.Arbitrary ()
import           Graphics.Vty.Attributes
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           Text.Megaparsec (errorBundlePretty)
import           Text.Megaparsec.Pos
import           Text.Mustache
import           Text.Mustache.Type ( showKey )
--------------------------------------------------------------------------------

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

instance Arbitrary PName where
  arbitrary = PName . pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary Key where
  arbitrary = Key <$> listOf1 arbSafeText
    where arbSafeText = pack <$> listOf1 (elements ['a'..'z'])
  shrink (Key []) = error "unreachable"
  shrink k@(Key [_]) = pure k
  shrink (Key (p:ps)) = Key . (p :) <$> shrink ps

instance Arbitrary Pos where
  arbitrary = mkPos . succ . abs <$> arbitrary
  shrink (unPos -> 1) = []
  shrink (unPos -> x) = mkPos <$> [x..1]

instance Arbitrary Node where
  arbitrary = sized node
    where
      node n | n > 0 = oneof $ leaves ++ branches (n `div` 2)
      node _ = oneof leaves
      branches n =
        [ Section <$> arbitrary <*> subnodes n
        , InvertedSection <$> arbitrary <*> subnodes n
        ]
      subnodes = fmap concatTextBlocks . listOf . node
      leaves =
        [ TextBlock . pack <$> listOf1 (elements ['a'..'z'])
        , EscapedVar <$> arbitrary
        , UnescapedVar <$> arbitrary
        -- TODO fix pretty-printing of mustache partials
        -- , Partial <$> arbitrary <*> arbitrary
        ]
  shrink = genericShrink

concatTextBlocks :: [Node] -> [Node]
concatTextBlocks [] = []
concatTextBlocks [x] = [x]
concatTextBlocks (TextBlock txt₁ : TextBlock txt₂ : xs)
  = concatTextBlocks $ TextBlock (txt₁ <> txt₂) : concatTextBlocks xs
concatTextBlocks (x : xs) = x : concatTextBlocks xs

instance Arbitrary Template where
  arbitrary = do
    template <- concatTextBlocks <$> arbitrary
    templateName <- arbitrary
    rest <- arbitrary
    pure $ Template
      { templateActual = templateName
      , templateCache = rest & at templateName ?~ template
      }
  shrink (Template actual cache) =
    let Just tpl = cache ^. at actual
    in do
      cache' <- shrink cache
      tpl' <- shrink tpl
      actual' <- shrink actual
      pure $ Template
        { templateActual = actual'
        , templateCache = cache' & at actual' ?~ tpl'
        }

instance CoArbitrary Template where
  coarbitrary = coarbitrary . ppTemplate

instance Function Template where
  function = functionMap ppTemplate parseTemplatePartial
    where
      parseTemplatePartial txt
        = compileMustacheText "template" txt ^?! _Right

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    pure $ x :| xs

instance CoArbitrary a => CoArbitrary (NonEmpty a) where
  coarbitrary = coarbitrary . toList

instance Function a => Function (NonEmpty a) where
  function = functionMap toList NonEmpty.fromList

ppNode :: Map PName [Node] -> Node -> Text
ppNode _ (TextBlock txt) = txt
ppNode _ (EscapedVar k) = "{{" <> showKey k <> "}}"
ppNode ctx (Section k body) =
  let sk = showKey k
  in "{{#" <> sk <> "}}" <> foldMap (ppNode ctx) body <> "{{/" <> sk <> "}}"
ppNode _ (UnescapedVar k) = "{{{" <> showKey k <> "}}}"
ppNode ctx (InvertedSection k body) =
  let sk = showKey k
  in "{{^" <> sk <> "}}" <> foldMap (ppNode ctx) body <> "{{/" <> sk <> "}}"
ppNode _ (Partial n _) = "{{> " <> unPName n <> "}}"

ppTemplate :: Template -> Text
ppTemplate (Template actual cache) =
  case cache ^. at actual of
    Nothing -> error "Template not found?"
    Just nodes -> foldMap (ppNode cache) nodes

instance ToJSON Template where
  toJSON = String . ppTemplate

instance FromJSON Template where
  parseJSON
    = withText "Template"
    $ either (fail . errorBundlePretty) pure
    . compileMustacheText "template"

instance CoArbitrary Text where
  coarbitrary = coarbitrary . unpack

instance Function Text where
  function = functionMap unpack pack

deriving anyclass instance NFData Node
deriving anyclass instance NFData Template

instance FromJSON Color where
  parseJSON = withText "Color" $ \case
    "black"   -> pure black
    "red"     -> pure red
    "green"   -> pure green
    "yellow"  -> pure yellow
    "blue"    -> pure blue
    "magenta" -> pure magenta
    "cyan"    -> pure cyan
    "white"   -> pure white
    _         -> fail "Invalid color"

instance ToJSON Color where
  toJSON color
    | color == black = "black"
    | color == red = "red"
    | color == green = "green"
    | color == yellow = "yellow"
    | color == blue = "blue"
    | color == magenta = "magenta"
    | color == cyan = "cyan"
    | color == white = "white"
    | otherwise = error "unimplemented"

instance (Eq a, Show a, Read a, FromJSON a) => FromJSON (MaybeDefault a) where
  parseJSON Null                   = pure Default
  parseJSON (String "keepCurrent") = pure KeepCurrent
  parseJSON x                      = SetTo <$> parseJSON x

instance ToJSON a => ToJSON (MaybeDefault a) where
  toJSON Default     = Null
  toJSON KeepCurrent = String "keepCurrent"
  toJSON (SetTo x)   = toJSON x

--------------------------------------------------------------------------------

instance Arbitrary Color where
  arbitrary = genericArbitrary

deriving anyclass instance CoArbitrary Color
deriving anyclass instance Function Color

instance (Eq a, Show a, Read a, Arbitrary a) => Arbitrary (MaybeDefault a) where
  arbitrary = oneof [ pure Default
                    , pure KeepCurrent
                    , SetTo <$> arbitrary
                    ]

instance CoArbitrary a => CoArbitrary (MaybeDefault a) where
  coarbitrary Default = variant @Int 1
  coarbitrary KeepCurrent = variant @Int 2
  coarbitrary (SetTo x) = variant @Int 3 . coarbitrary x

instance (Eq a, Show a, Read a, Function a) => Function (MaybeDefault a) where
  function = functionShow

instance Arbitrary Attr where
  arbitrary = do
    attrStyle <- arbitrary
    attrForeColor <- arbitrary
    attrBackColor <- arbitrary
    attrURL <- arbitrary
    pure Attr {..}

deriving anyclass instance CoArbitrary Attr
deriving anyclass instance Function Attr

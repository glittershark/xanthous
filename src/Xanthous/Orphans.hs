{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Xanthous.Orphans
  ( ppTemplate
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (elements, (.=))
--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.List.NonEmpty (NonEmpty(..))
import           Graphics.Vty.Attributes
import           Brick.Widgets.Edit
import           Data.Text.Zipper.Generic (GenericTextZipper)
import           Brick.Widgets.Core (getName)
import           System.Random.Internal (StdGen (..))
import           System.Random.SplitMix (SMGen ())
import           Test.QuickCheck
import           "quickcheck-instances" Test.QuickCheck.Instances ()
import           Text.Megaparsec (errorBundlePretty)
import           Text.Megaparsec.Pos
import           Text.Mustache
import           Text.Mustache.Type ( showKey )
import           Control.Monad.State
import           Linear
--------------------------------------------------------------------------------
import           Xanthous.Util.JSON
import           Xanthous.Util.QuickCheck
import qualified Data.Interval as Interval
import Data.Interval (Interval, Extended (..))
--------------------------------------------------------------------------------

instance forall s a.
  ( Cons s s a a
  , IsSequence s
  , Element s ~ a
  ) => Cons (NonNull s) (NonNull s) a a where
  _Cons = prism hither yon
    where
      hither :: (a, NonNull s) -> NonNull s
      hither (a, ns) =
        let s = toNullable ns
        in impureNonNull $ a <| s

      yon :: NonNull s -> Either (NonNull s) (a, NonNull s)
      yon ns = case nuncons ns of
        (_, Nothing) -> Left ns
        (x, Just xs) -> Right (x, xs)

instance forall a. Cons (NonEmpty a) (NonEmpty a) a a where
  _Cons = prism hither yon
    where
      hither :: (a, NonEmpty a) -> NonEmpty a
      hither (a, x :| xs) = a :| (x : xs)

      yon :: NonEmpty a -> Either (NonEmpty a) (a, NonEmpty a)
      yon ns@(x :| xs) = case xs of
        (y : ys) -> Right (x, y :| ys)
        [] -> Left ns


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
    -- templateName <- arbitrary
    -- rest <- arbitrary
    let templateName = "template"
        rest = mempty
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

deriving anyclass instance NFData Node
deriving anyclass instance NFData Template

instance FromJSON Color where
  parseJSON (String "black")         = pure black
  parseJSON (String "red")           = pure red
  parseJSON (String "green")         = pure green
  parseJSON (String "yellow")        = pure yellow
  parseJSON (String "blue")          = pure blue
  parseJSON (String "magenta")       = pure magenta
  parseJSON (String "cyan")          = pure cyan
  parseJSON (String "white")         = pure white
  parseJSON (String "brightBlack")   = pure brightBlack
  parseJSON (String "brightRed")     = pure brightRed
  parseJSON (String "brightGreen")   = pure brightGreen
  parseJSON (String "brightYellow")  = pure brightYellow
  parseJSON (String "brightBlue")    = pure brightBlue
  parseJSON (String "brightMagenta") = pure brightMagenta
  parseJSON (String "brightCyan")    = pure brightCyan
  parseJSON (String "brightWhite")   = pure brightWhite
  parseJSON n@(Number _)             = Color240 <$> parseJSON n
  parseJSON x                        = typeMismatch "Color" x

instance ToJSON Color where
  toJSON color
    | color == black         = "black"
    | color == red           = "red"
    | color == green         = "green"
    | color == yellow        = "yellow"
    | color == blue          = "blue"
    | color == magenta       = "magenta"
    | color == cyan          = "cyan"
    | color == white         = "white"
    | color == brightBlack   = "brightBlack"
    | color == brightRed     = "brightRed"
    | color == brightGreen   = "brightGreen"
    | color == brightYellow  = "brightYellow"
    | color == brightBlue    = "brightBlue"
    | color == brightMagenta = "brightMagenta"
    | color == brightCyan    = "brightCyan"
    | color == brightWhite   = "brightWhite"
    | Color240 num <- color  = toJSON num
    | otherwise             = error $ "unimplemented: " <> show color

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
  arbitrary = oneof [ Color240 <$> choose (0, 239)
                    , ISOColor <$> choose (0, 15)
                    ]

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

instance ToJSON Attr where
  toJSON Attr{..} = object
    [ "style" .= maybeDefaultToJSONWith styleToJSON attrStyle
    , "foreground" .= attrForeColor
    , "background" .= attrBackColor
    , "url" .= attrURL
    ]
    where
      maybeDefaultToJSONWith _ Default = Null
      maybeDefaultToJSONWith _ KeepCurrent = String "keepCurrent"
      maybeDefaultToJSONWith tj (SetTo x) = tj x
      styleToJSON style
        | style == standout     = "standout"
        | style == underline    = "underline"
        | style == reverseVideo = "reverseVideo"
        | style == blink        = "blink"
        | style == dim          = "dim"
        | style == bold         = "bold"
        | style == italic       = "italic"
        | otherwise            = toJSON style

instance FromJSON Attr where
  parseJSON = withObject "Attr" $ \obj -> do
    attrStyle <- parseStyle =<< obj .:? "style" .!= Default
    attrForeColor <- obj .:? "foreground" .!= Default
    attrBackColor <- obj .:? "background" .!= Default
    attrURL <- obj .:? "url" .!= Default
    pure Attr{..}

    where
      parseStyle (SetTo (String "standout"))     = pure (SetTo standout)
      parseStyle (SetTo (String "underline"))    = pure (SetTo underline)
      parseStyle (SetTo (String "reverseVideo")) = pure (SetTo reverseVideo)
      parseStyle (SetTo (String "blink"))        = pure (SetTo blink)
      parseStyle (SetTo (String "dim"))          = pure (SetTo dim)
      parseStyle (SetTo (String "bold"))         = pure (SetTo bold)
      parseStyle (SetTo (String "italic"))       = pure (SetTo italic)
      parseStyle (SetTo n@(Number _))            = SetTo <$> parseJSON n
      parseStyle (SetTo v)                       = typeMismatch "Style" v
      parseStyle Default                         = pure Default
      parseStyle KeepCurrent                     = pure KeepCurrent

deriving stock instance Ord Color
deriving stock instance Ord a => Ord (MaybeDefault a)
deriving stock instance Ord Attr

--------------------------------------------------------------------------------

instance (SemiSequence a, Arbitrary (Element a), Arbitrary a)
         => Arbitrary (NonNull a) where
  arbitrary = ncons <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (NonNull a) where
  toJSON = toJSON . toNullable

instance (FromJSON a, MonoFoldable a) => FromJSON (NonNull a) where
  parseJSON = maybe (fail "Found empty list") pure . fromNullable <=< parseJSON

instance NFData a => NFData (NonNull a) where
  rnf xs = xs `seq` toNullable xs `deepseq` ()

--------------------------------------------------------------------------------

instance forall t name. (NFData t, Monoid t, NFData name)
                 => NFData (Editor t name) where
  rnf ed = getName @_ @name ed `deepseq` getEditContents ed `deepseq` ()

deriving via (ReadShowJSON SMGen) instance ToJSON SMGen
deriving via (ReadShowJSON SMGen) instance FromJSON SMGen

instance ToJSON StdGen where
  toJSON = toJSON . unStdGen
  toEncoding = toEncoding . unStdGen

instance FromJSON StdGen where
  parseJSON = fmap StdGen . parseJSON

--------------------------------------------------------------------------------

instance CoArbitrary a => CoArbitrary (NonNull a) where
  coarbitrary = coarbitrary . toNullable

instance (MonoFoldable a, Function a) => Function (NonNull a) where
  function = functionMap toNullable $ fromMaybe (error "null") . fromNullable

instance (Arbitrary t, Arbitrary n, GenericTextZipper t)
       => Arbitrary (Editor t n) where
  arbitrary = editor <$> arbitrary <*> arbitrary <*> arbitrary

instance forall t n. (CoArbitrary t, CoArbitrary n, Monoid t)
              => CoArbitrary (Editor t n) where
  coarbitrary ed = coarbitrary (getName @_ @n ed, getEditContents ed)

instance CoArbitrary StdGen where
  coarbitrary = coarbitrary . show

instance Function StdGen where
  function = functionMap unStdGen StdGen

instance Function SMGen where
  function = functionShow

--------------------------------------------------------------------------------

deriving newtype instance (Arbitrary s, CoArbitrary (m (a, s)))
            => CoArbitrary (StateT s m a)

--------------------------------------------------------------------------------

deriving via (GenericArbitrary (V2 a)) instance Arbitrary a => Arbitrary (V2 a)
instance CoArbitrary a => CoArbitrary (V2 a)
instance Function a => Function (V2 a)

--------------------------------------------------------------------------------

instance Arbitrary r => Arbitrary (Extended r) where
  arbitrary = oneof [ pure NegInf
                    , pure PosInf
                    , Finite <$> arbitrary
                    ]

instance Arbitrary Interval.Boundary where
  arbitrary = elements [ Interval.Open , Interval.Closed ]

instance (Ord r, Arbitrary r) => Arbitrary (Interval r) where
  arbitrary = do
    lower <- arbitrary
    upper <- arbitrary
    pure $ (if upper < lower then flip else id)
      Interval.interval
      lower
      upper

{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
module Xanthous.Messages.Template
  ( -- * Template AST
    Template(..)
  , Substitution(..)
  , Filter(..)

    -- ** Template AST transformations
  , reduceTemplate

    -- * Template parser
  , template
  , runParser
  , errorBundlePretty

    -- * Template pretty-printer
  , ppTemplate

    -- * Rendering templates
  , TemplateVar(..)
  , nested
  , TemplateVars(..)
  , vars
  , RenderError
  , render
  )
where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding
                 (many, concat, try, elements, some, parts)
--------------------------------------------------------------------------------
import           Test.QuickCheck hiding (label)
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Instances.Semigroup ()
import           Test.QuickCheck.Checkers (EqProp)
import           Control.Monad.Combinators.NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Data
import           Text.Megaparsec hiding (sepBy1, some)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Function (fix)
--------------------------------------------------------------------------------
import Xanthous.Util (EqEqProp(..))
--------------------------------------------------------------------------------

genIdentifier :: Gen Text
genIdentifier = pack <$> listOf1 (elements identifierChars)

identifierChars :: String
identifierChars = ['a'..'z'] <> ['A'..'Z'] <> ['-', '_']

newtype Filter = FilterName Text
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (NFData)
  deriving (IsString) via Text

instance Arbitrary Filter where
  arbitrary = FilterName <$> genIdentifier
  shrink (FilterName fn) = fmap FilterName . filter (not . null) $ shrink fn

data Substitution
  = SubstPath (NonEmpty Text)
  | SubstFilter Substitution Filter
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (NFData)

instance Arbitrary Substitution where
  arbitrary = sized . fix $ \gen n ->
    let leaves =
          [ SubstPath <$> ((:|) <$> genIdentifier <*> listOf genIdentifier)]
        subtree = gen $ n `div` 2
    in if n == 0
       then oneof leaves
       else oneof $ leaves <> [ SubstFilter <$> subtree <*> arbitrary ]
  shrink (SubstPath pth) =
    fmap SubstPath
    . filter (not . any ((||) <$> null <*> any (`notElem` identifierChars)))
    $ shrink pth
  shrink (SubstFilter s f)
    = shrink s
    <> (uncurry SubstFilter <$> shrink (s, f))

data Template
  = Literal Text
  | Subst Substitution
  | Concat Template Template
  deriving stock (Show, Generic, Data)
  deriving anyclass (NFData)
  deriving EqProp via EqEqProp Template

instance Plated Template where
  plate _ tpl@(Literal _) = pure tpl
  plate _ tpl@(Subst _) = pure tpl
  plate f (Concat tpl₁ tpl₂) = Concat <$> f tpl₁ <*> f tpl₂

reduceTemplate :: Template -> Template
reduceTemplate = transform $ \case
  (Concat (Literal t₁) (Literal t₂)) -> Literal (t₁ <> t₂)
  (Concat (Literal "") t) -> t
  (Concat t (Literal "")) -> t
  (Concat t₁ (Concat t₂ t₃)) -> Concat (Concat t₁ t₂) t₃
  (Concat (Concat t₁ (Literal t₂)) (Literal t₃)) -> (Concat t₁ (Literal $ t₂ <> t₃))
  t -> t

instance Eq Template where
  tpl₁ == tpl₂ = case (reduceTemplate tpl₁, reduceTemplate tpl₂) of
    (Literal t₁, Literal t₂) -> t₁ == t₂
    (Subst s₁, Subst s₂) -> s₁ == s₂
    (Concat ta₁ ta₂, Concat tb₁ tb₂) -> ta₁ == tb₁ && ta₂ == tb₂
    _ -> False

instance Arbitrary Template where
  arbitrary = sized . fix $ \gen n ->
    let leaves = [ Literal . pack . filter (`notElem` ['\\', '{']) <$> arbitrary
                 , Subst <$> arbitrary
                 ]
        subtree = gen $ n `div` 2
        genConcat = Concat <$> subtree <*> subtree
    in if n == 0
       then oneof leaves
       else oneof $ genConcat : leaves
  shrink (Literal t) = Literal <$> shrink t
  shrink (Subst s) = Subst <$> shrink s
  shrink (Concat t₁ t₂)
    = shrink t₁
    <> shrink t₂
    <> (Concat <$> shrink t₁ <*> shrink t₂)

instance Semigroup Template where
  (<>) = Concat

instance Monoid Template where
  mempty = Literal ""

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identifier :: Parser Text
identifier = lexeme . label "identifier" $ do
  firstChar <- letterChar <|> oneOf ['-', '_']
  restChars <- many $ alphaNumChar <|> oneOf ['-', '_']
  pure $ firstChar <| pack restChars

filterName :: Parser Filter
filterName = FilterName <$> identifier

substitutionPath :: Parser Substitution
substitutionPath = SubstPath <$> sepBy1 identifier (char '.')

substitutionFilter :: Parser Substitution
substitutionFilter = do
  path <- substitutionPath
  fs <- some $ symbol "|" *> filterName
  pure $ foldl' SubstFilter path fs
  -- pure $ SubstFilter path f

substitutionContents :: Parser Substitution
substitutionContents
  =   try substitutionFilter
  <|> substitutionPath

substitution :: Parser Substitution
substitution = between (string "{{") (string "}}") substitutionContents

literal :: Parser Template
literal = Literal <$>
  (   (string "\\{" $> "{")
  <|> takeWhile1P Nothing (`notElem` ['\\', '{'])
  )

subst :: Parser Template
subst = Subst <$> substitution

template' :: Parser Template
template' = do
  parts <- many $ literal <|> subst
  pure $ foldr Concat (Literal "") parts


template :: Parser Template
template = reduceTemplate <$> template' <* eof

--------------------------------------------------------------------------------

ppSubstitution :: Substitution -> Text
ppSubstitution (SubstPath substParts) = intercalate "." substParts
ppSubstitution (SubstFilter s (FilterName f)) = ppSubstitution s <> " | " <> f

ppTemplate :: Template -> Text
ppTemplate (Literal txt) = txt
ppTemplate (Subst s) = "{{" <> ppSubstitution s <> "}}"
ppTemplate (Concat tpl₁ tpl₂) = ppTemplate tpl₁ <> ppTemplate tpl₂

--------------------------------------------------------------------------------

data TemplateVar
  = Val Text
  | Nested (Map Text TemplateVar)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

nested :: [(Text, TemplateVar)] -> TemplateVar
nested = Nested . mapFromList

instance Arbitrary TemplateVar where
  arbitrary = sized . fix $ \gen n ->
    let nst = fmap mapFromList . listOf $ (,) <$> arbitrary <*> gen (n `div` 2)
    in if n == 0
       then Val <$> arbitrary
       else oneof [ Val <$> arbitrary
                  , Nested <$> nst]

newtype TemplateVars = Vars { getTemplateVars :: Map Text TemplateVar }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving (Arbitrary) via (Map Text TemplateVar)

type instance Index TemplateVars = Text
type instance IxValue TemplateVars = TemplateVar
instance Ixed TemplateVars where
  ix k f (Vars vs) = Vars <$> ix k f vs
instance At TemplateVars where
  at k f (Vars vs) = Vars <$> at k f vs

vars :: [(Text, TemplateVar)] -> TemplateVars
vars = Vars . mapFromList

lookupVar :: TemplateVars -> NonEmpty Text -> Maybe TemplateVar
lookupVar vs (p :| []) = vs ^. at p
lookupVar vs (p :| (p₁ : ps)) = vs ^. at p >>= \case
  (Val _) -> Nothing
  (Nested vs') -> lookupVar (Vars vs') $ p₁ :| ps

data RenderError
  = NoSuchVariable (NonEmpty Text)
  | NestedFurther (NonEmpty Text)
  | NoSuchFilter Filter
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

renderSubst
  :: Map Filter (Text -> Text) -- ^ Filters
  -> TemplateVars
  -> Substitution
  -> Either RenderError Text
renderSubst _ vs (SubstPath pth) =
  case lookupVar vs pth of
    Just (Val v) -> Right v
    Just (Nested _) -> Left $ NestedFurther pth
    Nothing -> Left $ NoSuchVariable pth
renderSubst fs vs (SubstFilter s fn) =
  case fs ^. at fn of
    Just filterFn -> filterFn <$> renderSubst fs vs s
    Nothing -> Left $ NoSuchFilter fn

render
  :: Map Filter (Text -> Text) -- ^ Filters
  -> TemplateVars             -- ^ Template variables
  -> Template                 -- ^ Template
  -> Either RenderError Text
render _ _ (Literal s) = pure s
render fs vs (Concat t₁ t₂) = (<>) <$> render fs vs t₁ <*> render fs vs t₂
render fs vs (Subst s) = renderSubst fs vs s

--------------------------------------------------------------------------------
module Xanthous.Messages.TemplateSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
import Test.QuickCheck.Instances.Text ()
import Data.List.NonEmpty (NonEmpty(..))
import Data.Function (fix)
--------------------------------------------------------------------------------
import Xanthous.Messages.Template
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Messages.Template"
  [ testGroup "parsing"
    [ testProperty "literals" $ forAll genLiteral $ \s ->
        testParse template s === Right (Literal s)
    , parseCase "escaped curlies"
      "foo\\{"
      $ Literal "foo{"
    , parseCase "simple substitution"
      "foo {{bar}}"
      $ Literal "foo " `Concat` Subst (SubstPath $ "bar" :| [])
    , parseCase "substitution with filters"
      "foo {{bar | baz}}"
      $ Literal "foo "
      `Concat` Subst (SubstFilter (SubstPath $ "bar" :| [])
                                  (FilterName "baz"))
    , parseCase "substitution with multiple filters"
      "foo {{bar | baz | qux}}"
      $ Literal "foo "
      `Concat` Subst (SubstFilter (SubstFilter (SubstPath $ "bar" :| [])
                                                (FilterName "baz"))
                                  (FilterName "qux"))
    , parseCase "two substitutions and a literal"
      "{{a}}{{b}}c"
      $ Subst (SubstPath $ "a" :| [])
      `Concat` Subst (SubstPath $ "b" :| [])
      `Concat` Literal "c"
    , localOption (QuickCheckTests 10)
    $ testProperty "round-trips with ppTemplate" $ \tpl ->
        testParse template (ppTemplate tpl) === Right tpl
    ]
  , testBatch $ monoid @Template mempty
  , testGroup "rendering"
    [ testProperty "rendering literals renders literally"
      $ forAll genLiteral $ \s fs vs ->
        render fs vs (Literal s) === Right s
    , testProperty "rendering substitutions renders substitutions"
      $ forAll genPath $ \ident val fs ->
        let tpl = Subst (SubstPath ident)
            tvs = varsWith ident val
        in render fs tvs tpl === Right val
    , testProperty "filters filter" $ forAll genPath
      $ \ident filterName filterFn val ->
        let tpl = Subst (SubstFilter (SubstPath ident) filterName)
            fs = mapFromList [(filterName, filterFn)]
            vs = varsWith ident val
        in render fs vs tpl === Right (filterFn val)
    ]
  ]
  where
    genLiteral = pack . filter (`notElem` ['\\', '{']) <$> arbitrary
    parseCase name input expected =
      testCase name $ testParse template input @?= Right expected
    testParse p = over _Left errorBundlePretty . runParser p "<test>"
    genIdentifier = pack @Text <$> listOf1 (elements identifierChars)
    identifierChars = ['a'..'z'] <> ['A'..'Z'] <> ['-', '_']

    varsWith (p :| []) val = vars [(p, Val val)]
    varsWith (phead :| ps) val = vars . pure . (phead ,) . flip fix ps $
      \next pth -> case pth of
          [] -> Val val
          p : ps' -> nested [(p, next ps')]

    genPath = (:|) <$> genIdentifier <*> listOf genIdentifier

--

{-# LANGUAGE BlockArguments #-}
--------------------------------------------------------------------------------
module Xanthous.OrphansSpec where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Text.Mustache
import           Text.Megaparsec (errorBundlePretty)
import           Graphics.Vty.Attributes
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import           Xanthous.Orphans
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Orphans"
  [ localOption (QuickCheckTests 50)
  . localOption (QuickCheckMaxSize 10)
  $ testGroup "Template"
    [ testProperty "ppTemplate / compileMustacheText " \tpl ->
        let src = ppTemplate tpl
            res :: Either String Template
            res = over _Left errorBundlePretty
                $ compileMustacheText (templateActual tpl) src
            expected = templateCache tpl ^?! at (templateActual tpl)
        in
          counterexample (unpack src)
          $ Right expected === do
            (Template actual cache) <- res
            maybe (Left "Template not found") Right $ cache ^? at actual
    , testProperty "JSON round trip" $ \(tpl :: Template) ->
        counterexample (unpack $ ppTemplate tpl)
        $ JSON.decode (JSON.encode tpl) === Just tpl
    ]
  , testGroup "Attr"
    [ testProperty "JSON round trip" $ \(attr :: Attr) ->
        JSON.decode (JSON.encode attr) === Just attr
    ]
  ]

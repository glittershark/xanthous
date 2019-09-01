{-# LANGUAGE BlockArguments #-}
module Xanthous.OrphansSpec where

import Test.Prelude
import Xanthous.Orphans
import Text.Mustache
import Text.Megaparsec (errorBundlePretty)

import Xanthous.Orphans ()

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
    ]
  ]

module Xanthous.Util.InflectionSpec (main, test) where

import Test.Prelude
import Xanthous.Util.Inflection

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Util.Inflection"
  [ testGroup "toSentence"
    [ testCase "empty"  $ toSentence [] @?= ""
    , testCase "single" $ toSentence ["x"] @?= "x"
    , testCase "two"    $ toSentence ["x", "y"] @?= "x and y"
    , testCase "three"  $ toSentence ["x", "y", "z"] @?= "x, y, and z"
    , testCase "four"   $ toSentence ["x", "y", "z", "w"] @?= "x, y, z, and w"
    ]
  ]

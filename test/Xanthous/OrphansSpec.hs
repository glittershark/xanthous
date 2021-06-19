{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
--------------------------------------------------------------------------------
module Xanthous.OrphansSpec where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Text.Mustache
import           Text.Megaparsec (errorBundlePretty)
import           Graphics.Vty.Attributes
import qualified Data.Aeson as JSON
import           Data.Interval (Interval, (<=..<=), (<=..<), (<..<=))
import           Data.Aeson ( ToJSON(toJSON), object, Value(Array) )
import           Data.Aeson.Types (fromJSON)
import           Data.IntegerInterval (Extended(Finite))
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
    [ jsonRoundTrip @Attr ]
  , testGroup "Extended"
    [ jsonRoundTrip @(Extended Int) ]
  , testGroup "Interval"
    [ testGroup "JSON"
      [ jsonRoundTrip @(Interval Int)
      , testCase "parses a single value as a length-1 interval" $
          getSuccess (fromJSON $ toJSON (1 :: Int))
          @?= Just (Finite (1 :: Int) <=..<= Finite 1)
      , testCase "parses a pair of values as a single-ended interval" $
          getSuccess (fromJSON $ toJSON ([1, 2] :: [Int]))
          @?= Just (Finite (1 :: Int) <=..< Finite (2 :: Int))
      , testCase "parses the full included/excluded syntax" $
          getSuccess (fromJSON $ Array [ object [ "Excluded" JSON..= (1 :: Int) ]
                                       , object [ "Included" JSON..= (4 :: Int) ]
                                       ])
          @?= Just (Finite (1 :: Int) <..<= Finite (4 :: Int))
      , testCase "parses open/closed as aliases" $
          getSuccess (fromJSON $ Array [ object [ "Open" JSON..= (1 :: Int) ]
                                       , object [ "Closed" JSON..= (4 :: Int) ]
                                       ])
          @?= Just (Finite (1 :: Int) <..<= Finite (4 :: Int))
      ]
    ]
  ]
  where
    getSuccess :: JSON.Result a -> Maybe a
    getSuccess (JSON.Error _) = Nothing
    getSuccess (JSON.Success r) = Just r

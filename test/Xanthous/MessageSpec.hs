{-# LANGUAGE OverloadedLists #-}
module Xanthous.MessageSpec ( main, test ) where

import Test.Prelude
import Xanthous.Messages
import Data.Aeson
import Text.Mustache
import Control.Lens.Properties

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Messages"
  [ testGroup "Message"
    [ testGroup "JSON decoding"
      [ testCase "Single"
        $ decode "\"Test Single Template\""
        @?= Just (Single
                  $ compileMustacheText "template" "Test Single Template"
                  ^?! _Right)
      , testCase "Choice"
        $ decode "[\"Choice 1\", \"Choice 2\"]"
        @?= Just
            (Choice
            [ compileMustacheText "template" "Choice 1" ^?! _Right
            , compileMustacheText "template" "Choice 2" ^?! _Right
            ])
      ]
    ]
  , localOption (QuickCheckTests 50)
  . localOption (QuickCheckMaxSize 10)
  $ testGroup "MessageMap"
    [ testGroup "instance Ixed"
        [ testProperty "traversal laws" $ \k ->
            isTraversal $ ix @MessageMap k
        , testCase "preview when exists" $
          let
            Right tpl = compileMustacheText "foo" "bar"
            msg = Single tpl
            mm = Nested [("foo", Direct msg)]
          in mm ^? ix ["foo"] @?= Just msg
        ]
    , testGroup "lookupMessage"
      [ testProperty "is equivalent to preview ix" $ \msgMap path ->
          lookupMessage path msgMap === msgMap ^? ix path
      ]
    ]

  , testGroup "Messages"
    [ testCase "are all valid" $ messages `deepseq` pure ()
    ]

  , testGroup "Template"
    [ testGroup "eq"
      [ testProperty "reflexive" $ \(tpl :: Template) -> tpl == tpl
      ]
    ]
  ]

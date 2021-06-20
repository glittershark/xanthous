--------------------------------------------------------------------------------
module Xanthous.Game.PromptSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Game.Prompt
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Game.PromptSpec"
  [ testGroup "mkMenuItems"
    [ testCase "with duplicate items"
      $ mkMenuItems @[_] [('a', MenuOption @Int "a" 1), ('a', MenuOption "a" 2)]
        @?= mapFromList [('a', MenuOption "a" 1), ('b', MenuOption "a" 2)]
    ]
  ]

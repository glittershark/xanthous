--------------------------------------------------------------------------------
module Xanthous.CommandSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Command
--------------------------------------------------------------------------------
import           Data.Aeson (fromJSON, Value(String))
import qualified Data.Aeson as A
import           Graphics.Vty.Input (Key(..), Modifier(..))
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.CommandSpec"
  [ testGroup "keybindings"
    [ testCase "all are valid" $ keybindings `deepseq` pure ()
    , testProperty "all non-move commands are bound" $ \cmd ->
        let isn'tMove = case cmd of
                          Move _ -> False
                          StartAutoMove _ -> False
                          _ -> True
        in isn'tMove ==> member cmd commands
    ]
  , testGroup "instance FromJSON Keybinding" $
    [ ("q", Keybinding (KChar 'q') [])
    , ("<up>", Keybinding KUp [])
    , ("<left>", Keybinding KLeft [])
    , ("<right>", Keybinding KRight [])
    , ("<down>", Keybinding KDown [])
    , ("S-q", Keybinding (KChar 'q') [MShift])
    , ("C-S-q", Keybinding (KChar 'q') [MCtrl, MShift])
    , ("m-<UP>", Keybinding KUp [MMeta])
    , ("S", Keybinding (KChar 'S') [])
    ] <&> \(s, kb) ->
      testCase (fromString $ unpack s <> " -> " <> show kb)
       $ fromJSON (String s) @?= A.Success kb
  ]

--------------------------------------------------------------------------------
module Xanthous.Data.EntitiesSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import           Xanthous.Data.Entities
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data.Entities"
  [ testGroup "Collision"
    [ testProperty "JSON round-trip" $ \(c :: Collision) ->
        JSON.decode (JSON.encode c) === Just c
    , testGroup "JSON encoding examples"
      [ testCase "Stop" $ JSON.encode Stop @?= "\"Stop\""
      , testCase "Combat" $ JSON.encode Combat @?= "\"Combat\""
      ]
    ]
  , testGroup "EntityAttributes"
    [ testProperty "JSON round-trip" $ \(ea :: EntityAttributes) ->
        JSON.decode (JSON.encode ea) === Just ea
    ]
  ]

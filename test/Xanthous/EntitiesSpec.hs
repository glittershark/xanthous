--------------------------------------------------------------------------------
module Xanthous.EntitiesSpec where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import           Xanthous.Entities
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities"
  [ testGroup "EntityChar"
    [ testProperty "JSON round-trip" $ \(ec :: EntityChar) ->
        JSON.decode (JSON.encode ec) === Just ec
    ]
  ]

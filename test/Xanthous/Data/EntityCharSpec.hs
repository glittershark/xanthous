--------------------------------------------------------------------------------
module Xanthous.Data.EntityCharSpec where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import           Xanthous.Data.EntityChar
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data.EntityChar"
  [ testProperty "JSON round-trip" $ \(ec :: EntityChar) ->
      JSON.decode (JSON.encode ec) === Just ec
  ]

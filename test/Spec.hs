import Test.Prelude
import qualified Xanthous.DataSpec
import qualified Xanthous.Data.EntityMapSpec
import qualified Xanthous.GameSpec

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous"
  [ Xanthous.DataSpec.test
  , Xanthous.Data.EntityMapSpec.test
  , Xanthous.GameSpec.test
  ]

import Test.Prelude
import qualified Xanthous.DataSpec
import qualified Xanthous.Data.EntityMapSpec
import qualified Xanthous.GameSpec
import qualified Xanthous.MessageSpec
import qualified Xanthous.OrphansSpec
import qualified Xanthous.Entities.RawsSpec

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous"
  [ Xanthous.Data.EntityMapSpec.test
  , Xanthous.Entities.RawsSpec.test
  , Xanthous.GameSpec.test
  , Xanthous.MessageSpec.test
  , Xanthous.OrphansSpec.test
  , Xanthous.DataSpec.test
  ]

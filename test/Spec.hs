import Test.Prelude
import qualified Xanthous.Data.EntityMapSpec
import qualified Xanthous.DataSpec
import qualified Xanthous.Entities.RawsSpec
import qualified Xanthous.GameSpec
import qualified Xanthous.Generators.UtilSpec
import qualified Xanthous.MessageSpec
import qualified Xanthous.OrphansSpec
import qualified Xanthous.Util.GraphicsSpec
import qualified Xanthous.Util.InflectionSpec

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous"
  [ Xanthous.Data.EntityMapSpec.test
  , Xanthous.Entities.RawsSpec.test
  , Xanthous.GameSpec.test
  , Xanthous.Generators.UtilSpec.test
  , Xanthous.MessageSpec.test
  , Xanthous.OrphansSpec.test
  , Xanthous.DataSpec.test
  , Xanthous.Util.GraphicsSpec.test
  , Xanthous.Util.InflectionSpec.test
  ]

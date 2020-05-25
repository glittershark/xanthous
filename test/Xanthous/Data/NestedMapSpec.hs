--------------------------------------------------------------------------------
module Xanthous.Data.NestedMapSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Test.QuickCheck.Instances.Semigroup ()
--------------------------------------------------------------------------------
import qualified Xanthous.Data.NestedMap as NM
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data.NestedMap"
  [ testProperty "insert/lookup" $ \nm ks v ->
      let nm' = NM.insert ks v nm
      in counterexample ("inserted: " <> show nm')
         $ NM.lookup @Map @Int @Int ks nm' === Just (NM.Val v)
  ]

--------------------------------------------------------------------------------
module Xanthous.Data.MemoSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
import Test.QuickCheck.Instances.Text ()
--------------------------------------------------------------------------------
import Xanthous.Data.Memo
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data.MemoSpec"
  [ testGroup "getMemoized"
    [ testProperty "when key matches" $ \k v ->
        getMemoized @Int @Int k (memoizeWith k v) === Just v
    ]
  ]

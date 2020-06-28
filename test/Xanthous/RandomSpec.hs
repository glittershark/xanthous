--------------------------------------------------------------------------------
module Xanthous.RandomSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Control.Monad.Random
--------------------------------------------------------------------------------
import Xanthous.Random
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Random"
  [ testGroup "chooseSubset"
    [ testProperty "chooses a subset"
      $ \(l :: [Int]) (Positive (r :: Double)) -> randomTest $ do
        ss <- chooseSubset r l
        pure $ all (`elem` l) ss

    ]
  ]
  where
    randomTest prop = evalRandT prop . mkStdGen =<< arbitrary

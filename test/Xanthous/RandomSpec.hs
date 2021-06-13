--------------------------------------------------------------------------------
module Xanthous.RandomSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Control.Monad.Random
--------------------------------------------------------------------------------
import           Xanthous.Random
import           Xanthous.Orphans ()
import qualified Data.Interval as Interval
import           Data.Interval (Interval, Extended (Finite), (<=..<=))
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
  , testGroup "chooseRange"
    [ testProperty "chooses in the range"
      $ \(rng :: Interval Int) ->
        not (Interval.null rng)
        ==> randomTest ( do
                chooseRange rng >>= \case
                  Just r -> pure
                           . counterexample (show r)
                           $ r `Interval.member` rng
                  Nothing -> pure $ property Discard
            )
    , testProperty "nonEmpty range is never empty"
      $ \ (lower :: Int) (NonZero diff) -> randomTest $ do
        let upper = lower + diff
        r <- chooseRange (Finite lower <=..<= Finite upper)
        pure $ isJust r

    ]
  ]
  where
    randomTest prop = evalRandT prop . mkStdGen =<< arbitrary

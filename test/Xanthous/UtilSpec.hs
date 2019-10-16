module Xanthous.UtilSpec (main, test) where

import Test.Prelude
import Xanthous.Util

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Util"
  [ testGroup "smallestNotIn"
    [ testCase "examples" $ do
        smallestNotIn [7 :: Word, 3, 7] @?= 0
        smallestNotIn [7 :: Word, 0, 1, 3, 7] @?= 2
    , testProperty "returns an element not in the list" $ \(xs :: [Word]) ->
        smallestNotIn xs `notElem` xs
    , testProperty "pred return is in the list" $ \(xs :: [Word]) ->
        let res = smallestNotIn xs
        in res /= 0 ==> pred res `elem` xs
    , testProperty "ignores order" $ \(xs :: [Word]) ->
        forAll (shuffle xs) $ \shuffledXs ->
          smallestNotIn xs === smallestNotIn shuffledXs
    ]
  , testGroup "takeWhileInclusive"
    [ testProperty "takeWhileInclusive (const True) ≡ id"
      $ \(xs :: [Int]) -> takeWhileInclusive (const True) xs === xs
    ]
  ]

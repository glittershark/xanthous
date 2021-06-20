module Xanthous.UtilSpec (main, test) where

import Test.Prelude
import Xanthous.Util
import Control.Monad.State.Lazy (execState)

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
  , testGroup "endoTimes"
    [ testCase "endoTimes 4 succ 5"
      $ endoTimes (4 :: Int) succ (5 :: Int) @?= 9
    ]
  , testGroup "modifyKL"
    [ testCase "_1 += 1"
      $ execState (modifyKL _1 $ pure . succ) (1 :: Int, 2 :: Int) @?= (2, 2)
    ]
  , testGroup "removeFirst"
    [ testCase "example" $
      removeFirst @[Int] (> 5) [1..10] @?= [1, 2, 3, 4, 5, 7, 8, 9, 10]
    , testProperty "the result is the right length" $ \(xs :: [Int]) p ->
        length (removeFirst p xs) `elem` [length xs, length xs - 1]
    ]
  , testGroup "AlphaChar"
    [ testCase "succ 'z'" $ succ (AlphaChar 'z') @?= AlphaChar 'A'
    ]
  ]

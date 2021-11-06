{-# LANGUAGE PackageImports #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Level.UtilSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
import System.Random (mkStdGen)
import Control.Monad.Random (runRandT)
import Data.Array.ST (STUArray, runSTUArray, thaw)
import Data.Array.IArray (bounds, array)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array (Array, range, listArray, Ix)
import Control.Monad.ST (ST, runST)
import "checkers" Test.QuickCheck.Instances.Array ()
import Linear.V2
--------------------------------------------------------------------------------
import Xanthous.Util
import Xanthous.Data (width, height)
--------------------------------------------------------------------------------
import Xanthous.Generators.Level.Util
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

--------------------------------------------------------------------------------

newtype GenArray a b = GenArray (Array a b)
  deriving stock (Show, Eq)

instance (Ix a, Arbitrary a, CoArbitrary a, Arbitrary b)
       => Arbitrary (GenArray a b) where
  arbitrary = GenArray <$> do
    (mkElem :: a -> b) <- arbitrary
    minDims <- arbitrary
    maxDims <- arbitrary
    let bnds = (minDims, maxDims)
    pure $ listArray bnds $ mkElem <$> range bnds

test :: TestTree
test = testGroup "Xanthous.Generators.Util"
  [ testGroup "randInitialize"
    [ testProperty "returns an array of the correct dimensions"
      $ \dims seed aliveChance ->
        let gen = mkStdGen seed
            res = runSTUArray
                $ fmap fst
                $ flip runRandT gen
                $ randInitialize dims aliveChance
        in bounds res === (0, V2 (dims ^. width) (dims ^. height))
    ]
  , testGroup "numAliveNeighborsM"
    [ testProperty "maxes out at 8"
      $ \(GenArray (arr :: Array (V2 Word) Bool)) loc ->
        let
          act :: forall s. ST s Word
          act = do
            mArr <- thaw @_ @_ @_ @(STUArray s) arr
            numAliveNeighborsM mArr loc
          res = runST act
        in counterexample (show res) $ between 0 8 res
    , testCase "on the outer x edge" $
      let act :: forall s. ST s Word
          act = do
            cells <- thaw @_ @_ @_ @(STUArray s) $ array @Array @Bool @(V2 Word)
              (V2 0 0, V2 2 2)
              [ (V2 0 0, True),  (V2 1 0, True),  (V2 2 0, True)
              , (V2 0 1, False), (V2 1 1, False), (V2 2 1, True)
              , (V2 0 2, True),  (V2 1 2, True),  (V2 2 2, True)
              ]
            numAliveNeighborsM cells (V2 0 1)
          res = runST act
      in res @?= 7
    , testCase "on the outer y edge" $
      let act :: forall s. ST s Word
          act = do
            cells <- thaw @_ @_ @_ @(STUArray s) $ array @Array @Bool @(V2 Word)
              (V2 0 0, V2 2 2)
              [ (V2 0 0, True),  (V2 1 0, True),  (V2 2 0, True)
              , (V2 0 1, False), (V2 1 1, False), (V2 2 1, True)
              , (V2 0 2, True),  (V2 1 2, True),  (V2 2 2, True)
              ]
            numAliveNeighborsM cells (V2 1 0)
          res = runST act
      in res @?= 6
    ]
  , testGroup "numAliveNeighbors"
    [ testProperty "is equivalient to runST . numAliveNeighborsM . thaw" $
      \(GenArray (arr :: Array (V2 Word) Bool)) loc ->
        let
          act :: forall s. ST s Word
          act = do
            mArr <- thaw @_ @_ @_ @(STUArray s) arr
            numAliveNeighborsM mArr loc
          res = runST act
        in numAliveNeighbors arr loc === res
    , testCase "on the outer x edge" $
      let cells =
            array @Array @Bool @(V2 Word)
            (V2 0 0, V2 2 2)
            [ (V2 0 0, True),  (V2 1 0, True),  (V2 2 0, True)
            , (V2 0 1, False), (V2 1 1, False), (V2 2 1, True)
            , (V2 0 2, True),  (V2 1 2, True),  (V2 2 2, True)
            ]
      in numAliveNeighbors cells (V2 0 1) @?= 7
    , testCase "on the outer y edge" $
      let cells =
            array @Array @Bool @(V2 Word)
            (V2 0 0, V2 2 2)
            [ (V2 0 0, True),  (V2 1 0, True),  (V2 2 0, True)
            , (V2 0 1, False), (V2 1 1, False), (V2 2 1, True)
            , (V2 0 2, True),  (V2 1 2, True),  (V2 2 2, True)
            ]
      in numAliveNeighbors cells (V2 1 0) @?= 6
    ]
  , testGroup "cloneMArray"
      [ testCase "clones the array" $ runST $
          let
            go :: forall s. ST s Assertion
            go = do
              arr <- newArray @(STUArray s) (0 :: Int, 5) (1 :: Int)
              arr' <- cloneMArray @_ @(STUArray s) arr
              writeArray arr' 0 1234
              x <- readArray arr 0
              pure $ x @?= 1
          in go
      ]
  ]

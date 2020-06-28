--------------------------------------------------------------------------------
module Xanthous.RandomBench (benchmark, main) where
--------------------------------------------------------------------------------
import Bench.Prelude
--------------------------------------------------------------------------------
import Control.Parallel.Strategies
import Control.Monad.Random
--------------------------------------------------------------------------------
import Xanthous.Random
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [benchmark]

--------------------------------------------------------------------------------

benchmark :: Benchmark
benchmark = bgroup "Random"
  [ bgroup "chooseSubset"
    [ bench "serially" $
      nf (evalRand $ chooseSubset (0.5 :: Double) [1 :: Int ..1000000])
         (mkStdGen 1234)
    ]
  , bgroup "choose weightedBy"
    [ bench "serially" $
      nf (evalRand
          . choose
          . weightedBy (\n -> product [n, pred n .. 1])
          $ [1 :: Int ..1000000])
         (mkStdGen 1234)
    ]
  ]

--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Bench.Prelude
--------------------------------------------------------------------------------
import qualified Xanthous.RandomBench
import qualified Xanthous.Generators.UtilBench

main :: IO ()
main = defaultMain
  [ Xanthous.Generators.UtilBench.benchmark
  ]

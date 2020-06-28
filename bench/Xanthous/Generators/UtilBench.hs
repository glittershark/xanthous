--------------------------------------------------------------------------------
module Xanthous.Generators.UtilBench (benchmark, main) where
--------------------------------------------------------------------------------
import           Bench.Prelude
--------------------------------------------------------------------------------
import           Data.Array.IArray
import           Data.Array.Unboxed
import           System.Random (getStdGen)
--------------------------------------------------------------------------------
import           Xanthous.Generators.Util
import qualified Xanthous.Generators.CaveAutomata as CaveAutomata
import           Xanthous.Data (Dimensions'(..))
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [benchmark]

--------------------------------------------------------------------------------

benchmark :: Benchmark
benchmark = bgroup "Generators.Util"
  [ bgroup "floodFill"
    [ env (NFWrapper <$> cells) $ \(NFWrapper ir) ->
        bench "checkerboard" $ nf (floodFill ir) (1,0)
    ]
  ]
  where
    cells :: IO Cells
    cells = CaveAutomata.generate
      CaveAutomata.defaultParams
      (Dimensions 50 50)
      <$> getStdGen

newtype NFWrapper a = NFWrapper a

instance NFData (NFWrapper a) where
  rnf (NFWrapper x) = x `seq` ()

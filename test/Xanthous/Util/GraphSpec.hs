module Xanthous.Util.GraphSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Xanthous.Util.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph (labNodes, size, order)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Arbitrary
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Util.Graph"
  [ testGroup "mstSubGraph"
    [ testProperty "always produces a subgraph"
        $ \(CG _ (graph :: Gr Int Int)) ->
          let msg = mstSubGraph $ undir graph
          in counterexample (show msg)
            $ msg `isSubGraphOf` undir graph
    , testProperty "returns a graph with the same nodes"
        $ \(CG _ (graph :: Gr Int Int)) ->
          let msg = mstSubGraph graph
          in counterexample (show msg)
            $ labNodes msg === labNodes graph
    , testProperty "has nodes - 1 edges"
        $ \(CG _ (graph :: Gr Int Int)) ->
          order graph > 1 ==>
          let msg = mstSubGraph graph
          in counterexample (show msg)
            $ size msg === order graph - 1
    , testProperty "always produces a simple graph"
        $ \(CG _ (graph :: Gr Int Int)) ->
          let msg = mstSubGraph graph
          in counterexample (show msg) $ isSimple msg
    ]
  ]

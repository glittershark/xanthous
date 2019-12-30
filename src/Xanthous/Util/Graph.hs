--------------------------------------------------------------------------------
module Xanthous.Util.Graph where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.Graph.Inductive.Query.MST (msTree)
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Basic (undir)
import           Data.Set (isSubsetOf)
--------------------------------------------------------------------------------

mstSubGraph
  :: forall gr node edge. (DynGraph gr, Real edge, Show edge)
  => gr node edge -> gr node edge
mstSubGraph graph = insEdges mstEdges . insNodes (labNodes graph) $ Graph.empty
  where
    mstEdges = ordNub $ do
      LP path <- msTree $ undir graph
      case path of
        [] -> []
        [_] -> []
        ((n₂, edgeWeight) : (n₁, _) : _) ->
          pure (n₁, n₂, edgeWeight)

isSubGraphOf
  :: (Graph gr1, Graph gr2, Ord node, Ord edge)
  => gr1 node edge
  -> gr2 node edge
  -> Bool
isSubGraphOf graph₁ graph₂
  = setFromList (labNodes graph₁) `isSubsetOf` setFromList (labNodes graph₂)
  && setFromList (labEdges graph₁) `isSubsetOf` setFromList (labEdges graph₂)

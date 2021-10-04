module Dagre.Rank exposing (assignRanks)

import Dagre.Utils as DU
import Graph as G



{-
   This function assigns rank to all the Nodes of a Acyclic Graph
   The ranks are returned as List of List of Nodes.
   The nodes that are in the same list have same rank.
   The list that has lower index has lower rank
   For example [[5,0], [1], [4,3,2], [7,6], [8]],
   here both 5,0 have same rank, similarly 4,3,2.
   But [5,0] have lower rank as compared to [4,3,2]
-}


assignRanks : G.AcyclicGraph n e -> List DU.Layer
assignRanks g =
    G.heightLevels g
        |> List.map (\r -> List.map (\node -> node.node.id) r)

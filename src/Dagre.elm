module Dagre exposing (acg, runLayout)

import Dagre.Normalize as DN
import Dagre.Order as DO
import Dagre.Position as DP
import Dagre.Rank as DR
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G



{-
   Tags used in comments (relevant for developers working on this project)
   - TODO : need to implement this
   - Possible Future Bug : A code that might cause a bug in future,
                           discovered when some user uses it for some
                           corner case. (this need not be tested for now)
-}
{-
   main function that returns the Dagre Layout
   Positions and Control Points for splines.
   This package implements Sugiyama style graph drawing.
   The Sugiyama framework has the following phases in order of execution
   1. TODO : Making Graph Acyclic (Removing cycles)
   2. Rank/Layer Assignment
   3. Normalizing (Removing Long Edges)
   4. Vertex Ordering (Reducing Number of Edge Crossing)
   5. Coordinate Assignment
-}


runLayout : G.Graph n e -> ( Dict G.NodeId DU.Coordinates, Dict DU.Edge (List G.NodeId) )
runLayout graph =
    case G.checkAcyclic graph of
        Ok g ->
            let
                edges =
                    DU.getEdges graph

                rankList =
                    DR.assignRanks g

                ( ( newRankList, newEdges ), controlPoints ) =
                    DN.addDummyNodesAndSplitEdges ( rankList, edges )

                bestRankList =
                    DO.vertexOrder ( newRankList, newEdges )

                finalDict =
                    DP.position graph ( bestRankList, newEdges )
            in
            ( finalDict, controlPoints )

        _ ->
            ( Dict.empty, Dict.empty )


acg : G.Graph Int ()
acg =
    G.fromNodeLabelsAndEdgePairs
        [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
        [ ( 0, 1 )
        , ( 1, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 5, 8 )
        , ( 2, 6 )
        , ( 2, 7 )
        , ( 3, 6 )
        , ( 3, 7 )
        , ( 4, 7 )
        , ( 4, 8 )
        , ( 6, 8 )
        , ( 7, 8 )
        ]

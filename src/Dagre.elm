module Dagre exposing (acg, runLayout)

import Dagre.Acyclic as DAC
import Dagre.Attributes as DA
import Dagre.Normalize as DN
import Dagre.Order as DO
import Dagre.Position as DP
import Dagre.Rank as DR
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G


defaultConfig : DA.Config
defaultConfig =
    { rankDir = DA.TB
    , widthDict = Dict.empty
    , heightDict = Dict.empty
    , width = 0
    , height = 0
    , nodeSep = 50
    , edgeSep = 10
    , rankSep = 50
    , marginX = 0
    , marginY = 0
    }



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
   1. Making Graph Acyclic (Removing cycles)
   2. Rank/Layer Assignment
   3. Normalizing (Removing Long Edges)
   4. Vertex Ordering (Reducing Number of Edge Crossing)
   5. Coordinate Assignment
-}


runLayout : List DA.Attribute -> G.Graph n e -> ( Dict G.NodeId DU.Coordinates, Dict DU.Edge (List G.NodeId) )
runLayout edits graph =
    let
        config =
            List.foldl (\f a -> f a) defaultConfig edits

        ( newGraph, newAcyclicGraph, reversedEdges ) =
            DAC.run graph

        edges =
            DU.getEdges newGraph

        rankList =
            DR.assignRanks newAcyclicGraph

        ( ( newRankList, newEdges ), controlPoints ) =
            DN.addDummyNodesAndSplitEdges ( rankList, edges )

        bestRankList =
            DO.vertexOrder ( newRankList, newEdges )

        finalDict =
            DP.position config newGraph ( bestRankList, newEdges )

        finalControlPoints =
            DAC.undo (DU.getEdges graph) reversedEdges controlPoints
    in
    ( finalDict, finalControlPoints )


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

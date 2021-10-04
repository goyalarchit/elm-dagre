module Dagre exposing (acg, runLayout)

import Dagre.Order as DO
import Dagre.Position as DP
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G
import List.Extra as LE



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



{-
   The following functions are used for adding Dummy Nodes and Edges
-}
{-
   This function updates edges and add the splitted edges in to list.
-}


splitEdgeAndUpdateEdges : DU.Edge -> List G.NodeId -> List DU.Edge -> List DU.Edge
splitEdgeAndUpdateEdges ( from, to ) dummyNodes edges =
    let
        removedFromEdges =
            LE.remove ( from, to ) edges

        splitPath =
            List.concat [ [ from ], dummyNodes, [ to ] ]

        splitEdges =
            DU.getEdgesFromPath splitPath
    in
    List.append removedFromEdges splitEdges



{-
   The following function inserts K nodes into K subsequent Layers
   For example [[5,0], [1], [4,3,2], [7,6], [8]] is the initial layer and
   we want to insert [9,10] from rank 1 onwards, it will give
   [[5,0], [1,9], [4,3,2,10], [7,6], [8]]
-}


insertKNodesIntoKSubsequentLayers : List DU.Layer -> Int -> List G.NodeId -> List DU.Layer
insertKNodesIntoKSubsequentLayers rankLayers startRank dummyNodes =
    LE.indexedFoldl
        (\p e layers ->
            LE.updateAt (startRank + p) (\layer -> List.append layer [ e ]) layers
        )
        rankLayers
        dummyNodes



{-
   This function updates the whole rankLayers,Edges and Control points
   TODO : This function can have a potential bug, i.e. if both fromRank and toRank have
   negetive rankValues. Need to Deal with that.

-}


checkAndSplitMultiSpanEdge : DU.Edge -> ( ( List DU.Layer, G.NodeId ), ( List DU.Edge, Dict DU.Edge (List G.NodeId) ) ) -> ( ( List DU.Layer, G.NodeId ), ( List DU.Edge, Dict DU.Edge (List G.NodeId) ) )
checkAndSplitMultiSpanEdge ( from, to ) ( ( rankLayers, dummyId ), ( edges, controlPoints ) ) =
    let
        fromRank =
            DU.getRank from rankLayers

        toRank =
            DU.getRank to rankLayers
    in
    if toRank - fromRank > 1 then
        let
            newDummyId =
                dummyId + toRank - fromRank - 1

            dummyNodes =
                List.range dummyId (newDummyId - 1)

            newEdges =
                splitEdgeAndUpdateEdges ( from, to ) dummyNodes edges

            newControlPoints =
                Dict.update ( from, to ) (Maybe.map (\_ -> dummyNodes)) controlPoints

            newRankLayers =
                insertKNodesIntoKSubsequentLayers rankLayers (fromRank + 1) dummyNodes
        in
        ( ( newRankLayers, newDummyId ), ( newEdges, newControlPoints ) )

    else
        ( ( rankLayers, dummyId ), ( edges, controlPoints ) )



{-
   [5,0],[1] [(0,1), (5,4)]

   [1,9],[(5,9),(9,4)],(5,4) = [9]


-}
-- addDummyNodesAndSplitEdgesBetween : (DU.Layer,DU.Layer) -> List DU.Edge -> ((DU.Layer,List DU.Edge),Dict DU.Edge (List G.NodeId))
-- addDummyNodesAndSplitEdgesBetween (l1,l2) edges =
--     let
--         initDummyId =
--     in
{-
   This function adds dummy nodes and splits the edges
   The edges which span over multiple layers are split.
   The split points are called Control points for the edge
   and are added as dummy nodes
-}


addDummyNodesAndSplitEdges : ( List DU.Layer, List DU.Edge ) -> ( ( List DU.Layer, List DU.Edge ), Dict DU.Edge (List G.NodeId) )
addDummyNodesAndSplitEdges ( rankLayers, edges ) =
    let
        initDummyId =
            case List.concat rankLayers |> List.maximum of
                Just x ->
                    x + 1

                Nothing ->
                    1

        initControlPoints =
            Dict.fromList <| List.map (\e -> ( e, [] )) edges

        ( ( newRankLayers, newDummyId ), ( newEdges, newControlPoints ) ) =
            List.foldl
                checkAndSplitMultiSpanEdge
                ( ( rankLayers, initDummyId ), ( edges, initControlPoints ) )
                edges
    in
    ( ( newRankLayers, newEdges ), newControlPoints )



{-
   Position



-}
{-
   main function that returns the Dagre Layout
   Positions and Control Points for splines
-}


runLayout : G.Graph n e -> ( Dict G.NodeId DU.Coordinates, Dict DU.Edge (List G.NodeId) )
runLayout graph =
    case G.checkAcyclic graph of
        Ok g ->
            let
                edges =
                    DU.getEdges graph

                rankList =
                    assignRanks g

                ( ( newRankList, newEdges ), controlPoints ) =
                    addDummyNodesAndSplitEdges ( rankList, edges )

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

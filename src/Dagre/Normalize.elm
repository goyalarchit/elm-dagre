module Dagre.Normalize exposing (addDummyNodesAndSplitEdges)

import Dagre.Layer as DL
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G
import List.Extra as LE



{-
   This function adds dummy nodes and splits the edges
   The edges which span over multiple layers are split.
   The split points are called Control points for the edge
   and are added as dummy nodes
    -- example
    -- input : (Layering, edges)
    -- ([[4,0],[1],[2]], [(0,1), (1,2), (4,2)])
    -- output
    -- (([[4,0],[1,5],[2]], [(0,1), (1,2), (4,5), (5,2)]), (4,2) = [5])

-}


addDummyNodesAndSplitEdges : Maybe G.NodeId -> ( List DL.Layer, List DU.Edge ) -> ( ( List DL.Layer, List DU.Edge ), Dict DU.Edge (List G.NodeId) )
addDummyNodesAndSplitEdges maybeInitDummyNodeId ( rankLayers, edges ) =
    let
        initDummyId =
            case maybeInitDummyNodeId of
                Just x ->
                    x

                Nothing ->
                    rankLayers
                        |> List.concatMap DL.toList
                        |> List.maximum
                        |> Maybe.withDefault 0
                        |> (+) 1

        initControlPoints =
            Dict.fromList <| List.map (\e -> ( e, [] )) edges

        ( ( newRankLayers, _ ), ( newEdges, newControlPoints ) ) =
            List.foldl
                checkAndSplitMultiSpanEdge
                ( ( rankLayers, initDummyId ), ( edges, initControlPoints ) )
                edges
    in
    ( ( newRankLayers, newEdges ), newControlPoints )



{-
   The following functions are used for adding Dummy Nodes and Edges
-}
{-
   This function updates the whole rankLayers,Edges and Control points
   TODO : This function can have a potential bug, i.e. if both fromRank and toRank have
   negetive rankValues. Need to Deal with that.

-}


checkAndSplitMultiSpanEdge : DU.Edge -> ( ( List DL.Layer, G.NodeId ), ( List DU.Edge, Dict DU.Edge (List G.NodeId) ) ) -> ( ( List DL.Layer, G.NodeId ), ( List DU.Edge, Dict DU.Edge (List G.NodeId) ) )
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


insertKNodesIntoKSubsequentLayers : List DL.Layer -> Int -> List G.NodeId -> List DL.Layer
insertKNodesIntoKSubsequentLayers rankLayers startRank dummyNodes =
    LE.indexedFoldl
        (\p e layers ->
            LE.updateAt (startRank + p)
                (\layer ->
                    DL.append e layer
                )
                layers
        )
        rankLayers
        dummyNodes

module Dagre.Order.Barycenter exposing (..)

import Dagre.Utils as DU
import Graph as G
import List.Extra as LE


type FixedLayer
    = PreviousLayer
    | NextLayer


barycenter : List DU.Edge -> FixedLayer -> Int -> List DU.Layer -> List DU.Layer
barycenter edges fixedLayer movableLayerRank layering =
    let
        movableLayer =
            DU.getLayer movableLayerRank layering

        ( neighbourFn, adjLayer ) =
            case fixedLayer of
                PreviousLayer ->
                    ( DU.alongIncomingEdges edges, DU.getLayer (movableLayerRank - 1) layering )

                NextLayer ->
                    ( DU.alongOutgoingEdges edges, DU.getLayer (movableLayerRank + 1) layering )

        baryCenterValues =
            List.map (\n -> ( n, calcBarycenter n neighbourFn adjLayer )) movableLayer

        newOrder =
            List.sortBy Tuple.second baryCenterValues |> List.map Tuple.first
    in
    LE.setAt movableLayerRank newOrder layering



-- Debug.todo "return the Layering having rank movableLayerRank sorted by barycenter values"
{-
   helper function for calculating barycenter value for a node

-}


calcBarycenter : G.NodeId -> DU.NeighbourFn -> DU.Layer -> Float
calcBarycenter nodeId neighbourFn adjLayer =
    let
        adj_nodes =
            neighbourFn nodeId

        adj_positions =
            List.map (DU.getOrder adjLayer) adj_nodes

        -- check if any -1 are there, as there can be
    in
    if List.isEmpty adj_positions then
        -1

    else
        toFloat (List.sum adj_positions) / toFloat (List.length adj_positions)

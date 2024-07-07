module Dagre.Order.Barycenter exposing (FixedLayer(..), barycenter)

import Dagre.Layer as DL
import Dagre.Utils as DU
import Graph as G
import List.Extra as LE



{-
   Fixed Layer denote which layer is fixed corresponding to current movable layer rank
   for example if we have 3 consecutive ranks i-1, i, i+1
   considering i to be movable layer's rank
   then if fixed layer is i-1 then it is denoted by PreviousLayer
   else if fixed layer is i+1 then it is denoted by NextLayer
-}


type FixedLayer
    = PreviousLayer
    | NextLayer



{-
   This is baryCenter function, which uses barycenter heuristic to minimize crossings
   note : Neighbour Fun give either incoming or outgoing edge neighbor
   baryCenter :  List DU.Edge -> Neighbour Fun -> Int -> List DL.Layer -> List DL.Layer
   baryCenter edges neighbourFn rank layering =
    -- calculate Barycenter for all nodes of layering[rank] as List (G.NodeId,Barycenter Value)
    -- sort the List "(G.NodeId,Barycenter Value)" using second value and map to "List G.NodeId"
    -- replace the layering[rank] with above sorted list.
    -- TODO : [optional step] add the bias function from dagrejs.

-}


barycenter : List DU.Edge -> FixedLayer -> Int -> List DL.Layer -> List DL.Layer
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

        newOrder =
            movableLayer
                |> DL.sortBy (\n -> calcBarycenter n neighbourFn adjLayer)
    in
    LE.setAt movableLayerRank newOrder layering



{-
   helper function for calculating barycenter value for a node
   What is barycenter?
    - Just like median heuristic it assumes the average position
      of neighbours in fixed layer
    - For example if a node in movable layer has 3 neighbours with
      positions [1,2,5], then
        barycenter value    = Sum of positions/ Number of neighbours
                            = (1+2+5) / 3
                            = 2.67

-}


calcBarycenter : G.NodeId -> DU.NeighbourFn -> DL.Layer -> Float
calcBarycenter nodeId neighbourFn adjLayer =
    let
        adj_nodes =
            neighbourFn nodeId

        adj_positions =
            List.map (DU.getOrder adjLayer) adj_nodes

        -- Possible Future Bug : check if any -1 are there, as this can be introduce a
        -- possible error in barycenter value, finally a bug in crossing
        -- minimization algorithm
    in
    if List.isEmpty adj_positions then
        -1

    else
        toFloat (List.sum adj_positions) / toFloat (List.length adj_positions)

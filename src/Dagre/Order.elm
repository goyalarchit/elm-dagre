module Dagre.Order exposing (..)

import Dagre.Order.Barycenter as DOB
import Dagre.Order.CrossCount as DOC
import Dagre.Utils as DU
import Debug
import Dict exposing (Dict)
import Dict.Extra as DE
import Graph as G
import List.Extra as LE



{-
   This function returns the updated rankList such that, it minimizes the edge
   crossings between the layers.
   TODO : Implement the minimization Logic. It returns the default order.
   TODO : implement the  Jünger and Mutzel, "2-DU.Layer Straightline Crossing Minimization",

-}


vertexOrder : ( List DU.Layer, List DU.Edge ) -> List DU.Layer
vertexOrder ( layering, edges ) =
    let
        bestCC =
            DOC.crossCount ( layering, edges )
    in
    optimizeOrdering ( layering, edges ) bestCC ( 0, 0 )



-- Debug.todo "return final optimized Layering (order)"
{-
   The main loop that minimizes the crossing reductions
-}


optimizeOrdering : ( List DU.Layer, List DU.Edge ) -> Int -> ( Int, Int ) -> List DU.Layer
optimizeOrdering ( layering, edges ) bestCC ( iter, lastBest ) =
    if lastBest < 3 then
        let
            newLayering =
                sweepLayers ( layering, edges ) iter

            -- Add transpose function for adjacent node swapping heuristic
            newCC =
                DOC.crossCount ( newLayering, edges )
        in
        if newCC < bestCC then
            optimizeOrdering ( newLayering, edges ) newCC ( iter + 1, 0 )

        else
            optimizeOrdering ( newLayering, edges ) newCC ( iter + 1, lastBest + 1 )

    else
        layering



-- Debug.todo "return received layering as best layering"
{-
   sorts Layers from top to bottom or bottom to top based on Barycenter values
-}


sweepLayers : ( List DU.Layer, List DU.Edge ) -> Int -> List DU.Layer
sweepLayers ( layering, edges ) iter =
    let
        maxRank =
            List.length layering - 1
    in
    if modBy 2 iter == 0 then
        -- Apply BarryCenter Heuristic from layer 1 (0 based index) to Last layer
        List.foldl (DOB.barycenter edges DOB.PreviousLayer) layering (List.range 1 maxRank)
        -- Debug.todo "Add Sweep Layer Logic"

    else
        List.foldl (DOB.barycenter edges DOB.NextLayer) layering (List.range 0 (maxRank - 1) |> List.reverse)



-- Debug.todo "Add Sweep Layer Logic"
{-
   This is baryCenter function, should be inside the Barycanter.elm
   note : Neighbour Fun give either incoming or outgoing edge neighbor
   baryCenter :  List DU.Edge -> Neighbour Fun -> Int -> List DU.Layer -> List DU.Layer
   baryCenter edges neighbourFn rank layering =
    -- calculate Barycenter for all nodes of layering[rank] as List (G.NodeId,Barycenter Value)
    -- sort the List "(G.NodeId,Barycenter Value)" using second value and map to "List G.NodeId"
    -- replace the layering[rank] with above sorted list.
    -- [optional step] add the bias function from dagre.

-}

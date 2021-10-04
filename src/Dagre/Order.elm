module Dagre.Order exposing (..)

import Dagre.Order.Barycenter as DOB
import Dagre.Order.CrossCount as DOC
import Dagre.Order.Init as DOI
import Dagre.Utils as DU
import Debug
import Dict exposing (Dict)
import Dict.Extra as DE
import Graph as G
import List.Extra as LE



{-
   This function returns the updated rankList such that, it minimizes the edge
   crossings between the layers.
   This algorithm is taken from Gansner, et al., (1993) : "A Technique for Drawing Directed Graphs."
   For debugging and reference visit DagreJS implementation (commit with SHA-id '6355259')
   TODO : implement the  Jünger and Mutzel,
        "2-DU.Layer Straightline Crossing Minimization", It improves time complexity

-}


vertexOrder : ( List DU.Layer, List DU.Edge ) -> List DU.Layer
vertexOrder ( layering, edges ) =
    let
        initLayering =
            DOI.initOrder layering

        bestCC =
            DOC.crossCount ( initLayering, edges )
    in
    optimizeOrdering ( initLayering, edges ) bestCC ( 0, 0 )



{-
   The main loop that minimizes the crossing reductions
-}


optimizeOrdering : ( List DU.Layer, List DU.Edge ) -> Int -> ( Int, Int ) -> List DU.Layer
optimizeOrdering ( layering, edges ) bestCC ( iter, lastBest ) =
    if lastBest < 3 then
        let
            newLayering =
                sweepLayers ( layering, edges ) iter

            -- TODO: Add transpose function for adjacent node swapping heuristic
            newCC =
                DOC.crossCount ( newLayering, edges )
        in
        if newCC < bestCC then
            optimizeOrdering ( newLayering, edges ) newCC ( iter + 1, 0 )

        else
            optimizeOrdering ( layering, edges ) bestCC ( iter + 1, lastBest + 1 )

    else
        layering



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
        -- Applies BarryCenter Heuristic from layer 1 (0 based index) to Last layer
        List.foldl (DOB.barycenter edges DOB.PreviousLayer) layering (List.range 1 maxRank)

    else
        List.foldl (DOB.barycenter edges DOB.NextLayer) layering (List.range 0 (maxRank - 1) |> List.reverse)

module Dagre.Order.Transpose exposing (transpose)

import Dagre.Layer as DL
import Dagre.Order.CrossCount as DOC
import Dagre.Utils as DU
import List.Extra as LE



{-
   This function implements the transpose routine from crossing reduction algorithm
   In this function each adjacent pair of vertices is examined. Their
   order is switched if this reduces the number of crossings. The
   function crossing (v,w) simply counts the number of
   edge crossings if v appears to the left of w in their rank.
-}


transpose : List DU.Edge -> List DL.Layer -> List DL.Layer
transpose edges layering =
    let
        ( newLayering, improved ) =
            optimizeViaTranspose edges layering
    in
    if improved then
        transpose edges newLayering

    else
        layering


optimizeViaTranspose : List DU.Edge -> List DL.Layer -> ( List DL.Layer, Bool )
optimizeViaTranspose edges layering =
    let
        maxRank =
            List.length layering - 1

        ranks =
            List.range 0 maxRank

        ( newLayering, improved ) =
            List.foldl (optimizeLayer edges) ( layering, False ) ranks
    in
    ( newLayering, improved )



{-
   The following function traverses through all the layer and tries to
   optimize position of nodes by swapping adjacent nodes in each layer.
-}


optimizeLayer : List DU.Edge -> Int -> ( List DL.Layer, Bool ) -> ( List DL.Layer, Bool )
optimizeLayer edges rank ( layering, improved ) =
    let
        prevLayer =
            DU.getLayer (rank - 1) layering

        curLayer =
            DU.getLayer rank layering

        nextLayer =
            DU.getLayer (rank + 1) layering

        positions =
            List.range 0 (DL.length curLayer - 2)

        ( newCurLayer, newImproved ) =
            List.foldl (optimizeNodePosition edges ( prevLayer, nextLayer )) ( curLayer, improved ) positions
    in
    ( LE.setAt rank newCurLayer layering, newImproved )



{-
   The following function swaps the i^th node with i+1^th node and checks if
   it reduces the number of crossings
-}


optimizeNodePosition : List DU.Edge -> ( DL.Layer, DL.Layer ) -> Int -> ( DL.Layer, Bool ) -> ( DL.Layer, Bool )
optimizeNodePosition edges ( prevLayer, nextLayer ) i ( curLayer, improved ) =
    let
        newCurLayer =
            DL.swapAt i (i + 1) curLayer

        oldLayers =
            [ prevLayer, curLayer, nextLayer ]

        newLayers =
            [ prevLayer, newCurLayer, nextLayer ]
    in
    if DOC.crossCount ( newLayers, edges ) < DOC.crossCount ( oldLayers, edges ) then
        ( newCurLayer, True )

    else
        ( curLayer, improved )

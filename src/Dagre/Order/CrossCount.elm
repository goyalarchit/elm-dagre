module Dagre.Order.CrossCount exposing (crossCount)

import Dagre.Layer as DL
import Dagre.Utils as DU
import List.Extra as LE



{-
   Comparator Lexicographical Sorting of Edges
-}


lexSortEdge : ( Int, Int ) -> ( Int, Int ) -> Order
lexSortEdge ( f1, t1 ) ( f2, t2 ) =
    case compare f1 f2 of
        LT ->
            LT

        EQ ->
            compare t1 t2

        GT ->
            GT



{-
   This function maps edges to position of nodes in layers
   and returns the south end of the layers sorted Lexicographically
   For example

   Note the edges passed to this function must be directed only
   from layer 1 to layer 2, and no edges should that do not have both ends in
   l1, and l2 must not be passed
-}


mapAndSortEdges : ( DL.Layer, DL.Layer ) -> List DU.Edge -> List Int
mapAndSortEdges ( l1, l2 ) edges =
    let
        mappedEdges =
            List.map (DU.mapEdgeToOrder ( l1, l2 )) edges

        sortedEdges =
            List.sortWith lexSortEdge mappedEdges

        southernPoints =
            List.map (\( _, to ) -> to) sortedEdges
    in
    southernPoints



{- Swaps the element till the correct position is found -}
-- swapKey :
{-
   This function insert the pth element into correct position,
   considering all elements before p are sorted and from p are unsorted
   It also returns the number of inversion that p had
-}


insertIntoSortedWithInversions : Int -> Int -> ( Int, List Int ) -> ( Int, List Int )
insertIntoSortedWithInversions p e ( prevInversions, nodes ) =
    let
        ( sorted, unsorted ) =
            ( List.take p nodes, List.drop (p + 1) nodes )

        ( lesser, greater ) =
            ( LE.takeWhile (\n -> n <= e) sorted, LE.dropWhile (\n -> n <= e) sorted )

        finalNodes =
            List.concat [ lesser, [ e ], greater, unsorted ]
    in
    ( prevInversions + List.length greater, finalNodes )



{-
   Insertion Sort with Accumulator


-}


insertionSortWithInversionAccumulator : List Int -> ( Int, List Int )
insertionSortWithInversionAccumulator nodes =
    LE.indexedFoldl insertIntoSortedWithInversions ( 0, nodes ) nodes



{-
   BiLayer CrossCount : Returns number of crossing's between two layers
   The inversion count algorithm is the naive algorithm decribed in
   Barth, et al. "Simple and Efficient Bilayer Cross Counting"
   I still need to implement the efficient algorithm proposed in paper
   or search for an efficient algorithm for Pure Functional Language
   "Just a thought may be use some array from elm :
   read article dethroning the list :p"
-}


biLayerCrossCount : List DU.Edge -> ( DL.Layer, DL.Layer ) -> Int
biLayerCrossCount edges ( l1, l2 ) =
    let
        reqEdges =
            DU.getEdgesDirectedFromLayers ( l1, l2 ) edges

        reqSouthernPoints =
            mapAndSortEdges ( l1, l2 ) reqEdges

        ( totalCrossings, _ ) =
            insertionSortWithInversionAccumulator reqSouthernPoints
    in
    totalCrossings



{- counts Crossing edges for a rank list -}


crossCount : ( List DL.Layer, List DU.Edge ) -> Int
crossCount ( rankList, edges ) =
    let
        fromLayers =
            List.take (List.length rankList - 1) rankList

        toLayers =
            List.drop 1 rankList

        adjacentLayers =
            List.map2 (\l1 l2 -> ( l1, l2 )) fromLayers toLayers

        cc =
            List.map (biLayerCrossCount edges) adjacentLayers |> List.foldl (+) 0
    in
    cc

module Dagre.Position exposing (..)

import Dict exposing (Dict)
import List.Extra as LE
import Graph as G
import Dagre.Utils as DU

-- checkDummy: List (Int,Bool)-> Int -> Bool


findInnerSegmentAndMarkConflicts: DU.Layer -> Int -> Int -> ((Int,Int),List DU.Edge) -> ((Int,Int),List DU.Edge)
findInnerSegmentAndMarkConflicts prevLayerLength order nodeId ((k0,scanPos),type1Conflicts) =
    let

    in
        ((0,0),[])



type1VisitLayer: List DU.Edge -> (DU.Layer,DU.Layer) -> List DU.Edge
type1VisitLayer edges (l1,l2) = 
    let
        reqEdges = DU.getEdgesDirectedFromLayers (l1,l2) edges
        
    in
        []


findType1Conflicts: (List DU.Layer, List DU.Edge) -> List DU.Edge
findType1Conflicts (rankList, edges) =
    let
        adjacentLayers = DU.getAdjacentLayerPairs rankList

    in
        List.concat <| List.map (type1VisitLayer edges) adjacentLayers

preprocessing: (List DU.Layer, List DU.Edge) -> (List DU.Edge, List DU.Edge)
preprocessing (rankList, edges) = 
    ([], [])



position: (List DU.Layer, List DU.Edge) -> Dict G.NodeId DU.Coordinates
position (rankList,edges) =
    Dict.empty


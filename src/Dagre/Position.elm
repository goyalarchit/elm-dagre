module Dagre.Position exposing (..)

import Dict exposing (Dict)
import List.Extra as LE
import Graph as G
import Dagre.Utils as DU


checkType1Conflict : (Int,Int) -> Int -> Bool
checkType1Conflict (k0,k1) k =
    if k<k0 || k>k1 then
        True
    else
        False


markType1Conflicts:List DU.EdgeWithType -> (Int,Int) -> Int -> List DU.Edge
markType1Conflicts edges (k0,k1) l =
    let
        nonInnerEdges = DU.getInEdges l edges |> DU.filterEdgesByType DU.NonInner
        conflictingNonInnerEdges = List.filter (\(f,t) -> checkType1Conflict (k0,k1) f) nonInnerEdges
    in
        conflictingNonInnerEdges

findOtherInnerSegmentNode : List DU.EdgeWithType -> Int -> Maybe Int
findOtherInnerSegmentNode edges nodeId =
    let
        innerEdges = DU.getInEdges nodeId edges |> DU.filterEdgesByType DU.Inner 
        upperNodeOfInnerSegments = List.map Tuple.first innerEdges
    in
        List.minimum upperNodeOfInnerSegments



findInnerSegmentAndMarkConflicts:  (Int,Int) -> List DU.EdgeWithType -> Int -> ((Int,Int),List DU.Edge) -> ((Int,Int),List DU.Edge)
findInnerSegmentAndMarkConflicts (prevLayerLength,layerLength) edges l1 ((k0,scanPos),type1Conflicts) =
    let
        w = findOtherInnerSegmentNode edges l1
    in
        case (w,l1==layerLength-1)  of
            (Nothing,False) ->
                ((k0,scanPos),type1Conflicts)
            (Just k1,_) ->
                let
                    subLayer = List.range scanPos l1
                    newConflictsList = List.map (markType1Conflicts edges (k0,k1)) subLayer
                    newConflicts = List.concat newConflictsList
                    
                in
                    ((k1,l1+1),List.append type1Conflicts newConflicts)
            (Nothing,True) ->
                let
                    k1 = prevLayerLength-1
                    subLayer = List.range scanPos l1
                    newConflictsList = List.map (markType1Conflicts edges (k0,k1)) subLayer
                    newConflicts = List.concat newConflictsList
                in
                    ((k1,l1+1),List.append type1Conflicts newConflicts)
                

type1VisitLayer: List DU.EdgeWithType -> (DU.Layer,DU.Layer) -> List DU.Edge
type1VisitLayer edges (l1,l2) = 
    let
        reqEdges =  DU.getEdgesWithTypeDirectedFromLayers (l1,l2) edges 
                    |> List.map (DU.mapEdgeWithTypeToOrder (l1,l2))
        prevLayerLength = List.length l1
        layerLength = List.length l2
        rawType1Conflicts = List.foldl (findInnerSegmentAndMarkConflicts (prevLayerLength,layerLength) reqEdges ) ((0,0),[]) (List.range 0 (layerLength-1)) 
                            |> Tuple.second
        
        type1Conflicts = List.map (DU.mapEdgeOrderToNode (l1,l2) ) rawType1Conflicts
        
    in
        type1Conflicts


findType1Conflicts: (List DU.Layer, List DU.EdgeWithType) -> List DU.Edge
findType1Conflicts (rankList, edges) =
    let
        adjacentLayers = DU.getAdjacentLayerPairs rankList

    in
        List.concat <| List.map (type1VisitLayer edges) adjacentLayers


{-
This is the implementation of Algorithm 1 from Brandes and kopf
The following function implements the preprocessing step. The algorithm marks  
all the type1 conflicts and type2(to be implemented) conflicts.

-}


preprocessing: (List DU.Layer, List DU.EdgeWithType) -> (List DU.Edge, List DU.Edge)
preprocessing (rankList, edges) =
    let
        allType1Conflicts = findType1Conflicts (rankList,edges)
    in
    
    (allType1Conflicts, [])



position: (List DU.Layer, List DU.EdgeWithType) -> Dict G.NodeId DU.Coordinates
position (rankList,edges) =
    Dict.empty

{-
map all the nodes in adjacent layer to their order, and the edges to orders too


after marking one layer's conflicting edges remap them to vertices

-}
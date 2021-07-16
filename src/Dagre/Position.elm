module Dagre.Position exposing (..)

import Dict exposing (Dict)
import Dict.Extra as DE
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



{-
The functions that start from here are helper function for
Vertical Alignment Function  
based on Algorithm-2 of Brandes and Kopf 
-}

type alias NeighbourFn = 
    (G.NodeId -> List G.NodeId)

type alias NodeDict = 
    Dict G.NodeId G.NodeId

getPosDict: List DU.Layer -> Dict G.NodeId Int
getPosDict rankList = 
    let
        dictList = List.map (\l -> List.map (\n -> (n,(DU.getOrder l n))) l ) rankList
                    |> List.concat
    in
        Dict.fromList dictList


getPos : Dict G.NodeId Int -> G.NodeId -> Int
getPos pos node = 
    case Dict.get node pos of 
        Just idx ->
            idx
        Nothing ->
            -1


getNode : G.NodeId -> NodeDict -> G.NodeId
getNode node dict =
    case Dict.get node dict of
        Just x ->
            x
        Nothing ->
            DU.intMin

hasConflict: List DU.Edge -> (G.NodeId,G.NodeId) -> Bool
hasConflict conflicts (u,v) =
    if (List.member (u,v) conflicts || List.member (v,u) conflicts) then
        True
    else
        False

alignVertexHelper: List DU.Edge -> G.NodeId -> (G.NodeId,Int) -> ((NodeDict,NodeDict),Int) -> ((NodeDict,NodeDict),Int)
alignVertexHelper conflicts v (w, pos_w) ((root,align),prevIdx) =
    if ((getNode v align) == v && prevIdx < pos_w && (hasConflict conflicts (w,v) |> not)) then
        let
            updatedAlignW = Dict.update w (Maybe.map (\_ -> v)) align
            updatedRootV = Dict.update v (Maybe.map (\_ -> (getNode w root))) root
            updatedAlignV = Dict.update v (Maybe.map (\_ -> (getNode v updatedRootV))) updatedAlignW
            newPrevIdx = pos_w

        in
            ((updatedRootV,updatedAlignV),newPrevIdx)
    else
        ((root,align),prevIdx)

            



alignVertex: Dict G.NodeId Int -> List DU.Edge -> NeighbourFn -> G.NodeId ->  ((NodeDict,NodeDict),Int) -> ((NodeDict,NodeDict),Int)
alignVertex pos conflicts neighbourFn v ((root,align),prevIdx) =
    let
        ws = neighbourFn v |> List.sortBy (getPos pos) 
        mp = (toFloat (List.length ws) - 1) / 2
        w_mp =  List.range (floor mp) (ceiling mp) 
                |> List.map (\i -> case (LE.getAt i ws) of 
                                        Just w -> w
                                        Nothing -> DU.intMin )
                |> List.filter (\w -> w /= DU.intMin )
                |> List.map (\w -> (w,(getPos pos w)))

        updatedValues = List.foldl (alignVertexHelper conflicts v) ((root,align),prevIdx) w_mp
    in
        updatedValues
    


verticalAlignmentVisitLayer : Dict G.NodeId Int -> List DU.Edge -> NeighbourFn -> DU.Layer -> (NodeDict, NodeDict) -> (NodeDict, NodeDict) 
verticalAlignmentVisitLayer pos conflicts neighbourFn layer (root,align) = 
    let
        ((finalRoot,finalAlign),_) = List.foldl (alignVertex pos conflicts neighbourFn) ((root,align),-1) layer
    in
        (finalRoot,finalAlign)



{-
This is the implementation of algorithm 2 of Brandes-Kopf
This function groups the nodes into blocks and returns two circular
linked lists, root and align, which is used for the actual horizontal 
Coordinate assignment.
-}
verticalAlignment: List DU.Layer -> List DU.Edge -> NeighbourFn -> (NodeDict, NodeDict)
verticalAlignment rankList conflicts neighbourFn =
    let
        root = DE.fromListBy identity (List.concat rankList)
        align = DE.fromListBy identity (List.concat rankList)
        pos = getPosDict rankList
        (finalRoot,finalAlign) = List.foldl (verticalAlignmentVisitLayer pos conflicts neighbourFn) (root,align) rankList
    in
        (finalRoot,finalAlign)



{-
This is the implementation of algorithm 3 of Brandes-Kopf
This function performs the horizontal compaction and Coordinate assignment.
-}






position: (List DU.Layer, List DU.EdgeWithType) -> Dict G.NodeId DU.Coordinates
position (rankList,edges) =
    Dict.empty

{-
map all the nodes in adjacent layer to their order, and the edges to orders too


after marking one layer's conflicting edges remap them to vertices

-}
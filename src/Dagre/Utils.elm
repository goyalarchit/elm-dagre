module Dagre.Utils exposing (..)

import Graph as G
import List.Extra as LE

type alias Coordinates = 
    (Float,Float)

type alias Edge = 
    (G.NodeId, G.NodeId)


type alias Layer =
    List G.NodeId



getEdges : G.Graph n e -> List Edge
getEdges g =
    let
        edges = G.edges g 
    in
        List.map (\e -> (e.from,e.to) ) edges


alongOutgoingEdges : G.NodeId -> List Edge -> List G.NodeId
alongOutgoingEdges nodeId edges =
    List.filter (\e -> Tuple.first e == nodeId ) edges
    |> List.map (\e -> Tuple.second e)

alongIngoingEdges : G.NodeId -> List Edge -> List G.NodeId
alongIngoingEdges nodeId edges =
    List.filter (\e -> Tuple.second e == nodeId ) edges
    |> List.map (\e -> Tuple.first e)


rank : G.NodeId -> List Layer -> Int
rank nodeId layers =
    case (LE.findIndex (List.member nodeId) layers) of
        Just x ->
            x
        Nothing ->
            -1

getEdgesFromPath : List G.NodeId -> List Edge
getEdgesFromPath path =
    let
        froms = List.take (List.length path-1) path
        tos = List.drop 1 path

    in
        List.map2 (\from to -> (from,to)) froms tos


{-
This function returns the index of a node in a layer, 
if the node does not exist then it returns -1
-}
getOrder : Layer -> G.NodeId -> Int
getOrder l nodeId =
    case (LE.elemIndex nodeId l) of
        Just idx -> 
            idx
        Nothing ->
            -1


mapEdgeToOrder : (Layer,Layer) -> Edge -> Edge
mapEdgeToOrder (l1,l2) e =
    Tuple.mapBoth (getOrder l1) (getOrder l2) e

getEdgesDirectedFromLayers : (Layer,Layer) -> List Edge -> List Edge
getEdgesDirectedFromLayers (l1,l2) edges =
    List.filter (\(from,to) -> (List.member from l1) &&  (List.member to l2) ) edges

getAdjacentLayerPairs: List Layer -> List (Layer,Layer)
getAdjacentLayerPairs rankList =
    let
        fromLayers = List.take ((List.length rankList)-1) rankList
        toLayers = List.drop 1 rankList

    in
        List.map2 (\l1 l2 -> (l1,l2)) fromLayers toLayers


checkDummyNode: G.NodeId -> G.NodeId -> Bool
checkDummyNode initDummyId nodeId =
    if nodeId < initDummyId then
        False
    else
        True
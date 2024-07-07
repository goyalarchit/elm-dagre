module Dagre.Utils exposing (Coordinates, Edge, EdgeType(..), EdgeWithType, NeighbourFn, alongIncomingEdges, alongOutgoingEdges, filterEdgesByType, getAdjacentLayerPairs, getEdgeType, getEdges, getEdgesDirectedFromLayers, getEdgesFromPath, getEdgesWithTypeDirectedFromLayers, getInEdges, getLayer, getNodeFromOrder, getOrder, getRank, infinity, intMax, intMin, isDummyNode, mapEdgeOrderToNode, mapEdgeToOrder, mapEdgeWithTypeToNodes, mapEdgeWithTypeToOrder, markEdgeWithEdgeType, markEdgesWithEdgeType)

import Dagre.Layer as DL
import Graph as G
import List.Extra as LE


intMin : Int
intMin =
    -2 ^ 31


intMax : Int
intMax =
    2 ^ 31 - 1


infinity : Float
infinity =
    2 ^ 31 - 1


type alias Coordinates =
    ( Float, Float )


type alias Edge =
    ( G.NodeId, G.NodeId )


type EdgeType
    = Inner
    | NonInner


type alias EdgeWithType =
    ( Edge, EdgeType )


type alias NeighbourFn =
    G.NodeId -> List G.NodeId


getEdges : G.Graph n e -> List Edge
getEdges g =
    let
        edges =
            G.edges g
    in
    List.map (\e -> ( e.from, e.to )) edges


alongOutgoingEdges : List Edge -> G.NodeId -> List G.NodeId
alongOutgoingEdges edges nodeId =
    List.filter (\e -> Tuple.first e == nodeId) edges
        |> List.map (\e -> Tuple.second e)


alongIncomingEdges : List Edge -> G.NodeId -> List G.NodeId
alongIncomingEdges edges nodeId =
    List.filter (\e -> Tuple.second e == nodeId) edges
        |> List.map (\e -> Tuple.first e)


getInEdges : G.NodeId -> List EdgeWithType -> List EdgeWithType
getInEdges nodeId edges =
    List.filter (\e -> (Tuple.first e |> Tuple.second) == nodeId) edges


getRank : G.NodeId -> List DL.Layer -> Int
getRank nodeId layers =
    case LE.findIndex (\layer -> DL.member nodeId layer) layers of
        Just x ->
            x

        Nothing ->
            -1


getEdgesFromPath : List G.NodeId -> List Edge
getEdgesFromPath path =
    let
        froms =
            List.take (List.length path - 1) path

        tos =
            List.drop 1 path
    in
    List.map2 (\from to -> ( from, to )) froms tos



{-
   This function returns the index of a node in a layer,
   if the node does not exist then it returns -1
-}


getOrder : DL.Layer -> G.NodeId -> Int
getOrder l nodeId =
    case DL.toOrder nodeId l of
        Just idx ->
            idx

        Nothing ->
            -1


mapEdgeToOrder : ( DL.Layer, DL.Layer ) -> Edge -> Edge
mapEdgeToOrder ( l1, l2 ) e =
    Tuple.mapBoth (getOrder l1) (getOrder l2) e


mapEdgeWithTypeToOrder : ( DL.Layer, DL.Layer ) -> EdgeWithType -> EdgeWithType
mapEdgeWithTypeToOrder ( l1, l2 ) e =
    Tuple.mapFirst (mapEdgeToOrder ( l1, l2 )) e


getNodeFromOrder : DL.Layer -> Int -> G.NodeId
getNodeFromOrder l order =
    case DL.toId order l of
        Just id ->
            id

        Nothing ->
            intMin


mapEdgeOrderToNode : ( DL.Layer, DL.Layer ) -> Edge -> Edge
mapEdgeOrderToNode ( l1, l2 ) e =
    Tuple.mapBoth (getNodeFromOrder l1) (getNodeFromOrder l2) e


mapEdgeWithTypeToNodes : ( DL.Layer, DL.Layer ) -> EdgeWithType -> EdgeWithType
mapEdgeWithTypeToNodes ( l1, l2 ) e =
    Tuple.mapFirst (mapEdgeOrderToNode ( l1, l2 )) e


getEdgesDirectedFromLayers : ( DL.Layer, DL.Layer ) -> List Edge -> List Edge
getEdgesDirectedFromLayers ( l1, l2 ) edges =
    List.filter (\( from, to ) -> DL.member from l1 && DL.member to l2) edges


getEdgesWithTypeDirectedFromLayers : ( DL.Layer, DL.Layer ) -> List EdgeWithType -> List EdgeWithType
getEdgesWithTypeDirectedFromLayers ( l1, l2 ) edges =
    List.filter (\( ( from, to ), _ ) -> DL.member from l1 && DL.member to l2) edges


getAdjacentLayerPairs : List DL.Layer -> List ( DL.Layer, DL.Layer )
getAdjacentLayerPairs rankList =
    let
        fromLayers =
            List.take (List.length rankList - 1) rankList

        toLayers =
            List.drop 1 rankList
    in
    List.map2 (\l1 l2 -> ( l1, l2 )) fromLayers toLayers


getLayer : Int -> List DL.Layer -> DL.Layer
getLayer rank layering =
    let
        layer =
            LE.getAt rank layering
    in
    Maybe.withDefault DL.empty layer


isDummyNode : G.NodeId -> G.NodeId -> Bool
isDummyNode initDummyId nodeId =
    if nodeId < initDummyId then
        False

    else
        True


getEdgeType : EdgeWithType -> EdgeType
getEdgeType edge =
    Tuple.second edge


markEdgeWithEdgeType : Int -> Edge -> EdgeWithType
markEdgeWithEdgeType initDummyId e =
    let
        ( from, to ) =
            e
    in
    if (from >= initDummyId) && (to >= initDummyId) then
        ( e, Inner )

    else
        ( e, NonInner )


markEdgesWithEdgeType : G.Graph n e -> List Edge -> List EdgeWithType
markEdgesWithEdgeType g edges =
    let
        initDummyId =
            case List.map (\n -> n.id) (G.nodes g) |> List.maximum of
                Just x ->
                    x + 1

                Nothing ->
                    -1
    in
    List.map (markEdgeWithEdgeType initDummyId) edges


filterEdgesByType : EdgeType -> List EdgeWithType -> List Edge
filterEdgesByType eType edges =
    List.filter (\e -> Tuple.second e == eType) edges
        |> List.map (\fe -> Tuple.first fe)

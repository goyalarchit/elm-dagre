module Dagre.Utils exposing (..)

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


type alias Layer =
    List G.NodeId


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


getRank : G.NodeId -> List Layer -> Int
getRank nodeId layers =
    case LE.findIndex (List.member nodeId) layers of
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


getOrder : Layer -> G.NodeId -> Int
getOrder l nodeId =
    case LE.elemIndex nodeId l of
        Just idx ->
            idx

        Nothing ->
            -1


mapEdgeToOrder : ( Layer, Layer ) -> Edge -> Edge
mapEdgeToOrder ( l1, l2 ) e =
    Tuple.mapBoth (getOrder l1) (getOrder l2) e


mapEdgeWithTypeToOrder : ( Layer, Layer ) -> EdgeWithType -> EdgeWithType
mapEdgeWithTypeToOrder ( l1, l2 ) e =
    Tuple.mapFirst (mapEdgeToOrder ( l1, l2 )) e


getNodeFromOrder : Layer -> Int -> G.NodeId
getNodeFromOrder l order =
    case LE.getAt order l of
        Just n ->
            n

        Nothing ->
            intMin


mapEdgeOrderToNode : ( Layer, Layer ) -> Edge -> Edge
mapEdgeOrderToNode ( l1, l2 ) e =
    Tuple.mapBoth (getNodeFromOrder l1) (getNodeFromOrder l2) e


mapEdgeWithTypeToNodes : ( Layer, Layer ) -> EdgeWithType -> EdgeWithType
mapEdgeWithTypeToNodes ( l1, l2 ) e =
    Tuple.mapFirst (mapEdgeOrderToNode ( l1, l2 )) e


getEdgesDirectedFromLayers : ( Layer, Layer ) -> List Edge -> List Edge
getEdgesDirectedFromLayers ( l1, l2 ) edges =
    List.filter (\( from, to ) -> List.member from l1 && List.member to l2) edges


getEdgesWithTypeDirectedFromLayers : ( Layer, Layer ) -> List EdgeWithType -> List EdgeWithType
getEdgesWithTypeDirectedFromLayers ( l1, l2 ) edges =
    List.filter (\( ( from, to ), _ ) -> List.member from l1 && List.member to l2) edges


getAdjacentLayerPairs : List Layer -> List ( Layer, Layer )
getAdjacentLayerPairs rankList =
    let
        fromLayers =
            List.take (List.length rankList - 1) rankList

        toLayers =
            List.drop 1 rankList
    in
    List.map2 (\l1 l2 -> ( l1, l2 )) fromLayers toLayers


getLayer : Int -> List Layer -> Layer
getLayer rank layering =
    let
        layer =
            LE.getAt rank layering
    in
    Maybe.withDefault [] layer


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

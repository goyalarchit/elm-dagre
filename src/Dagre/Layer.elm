module Dagre.Layer exposing (Layer, append, empty, fromList, length, member, orderDict, reverse, sortBy, swapAt, toId, toList, toOrder)

import Dict exposing (Dict)
import Graph as G


type Layer
    = Layer Int (Dict G.NodeId Int) (Dict Int G.NodeId)


reverse : Layer -> Layer
reverse (Layer size toOrderDict _) =
    let
        ( newToOrderDict, newToIdDict ) =
            Dict.foldl
                (\id order ( oacc, iacc ) ->
                    let
                        newOrder : Int
                        newOrder =
                            size - order - 1
                    in
                    ( Dict.insert id newOrder oacc
                    , Dict.insert newOrder id iacc
                    )
                )
                ( Dict.empty, Dict.empty )
                toOrderDict
    in
    Layer size newToOrderDict newToIdDict


member : G.NodeId -> Layer -> Bool
member id (Layer _ toOrderDict _) =
    Dict.member id toOrderDict


toOrder : G.NodeId -> Layer -> Maybe Int
toOrder id (Layer _ toOrderDict _) =
    Dict.get id toOrderDict


toId : Int -> Layer -> Maybe G.NodeId
toId id (Layer _ _ toIdDict) =
    Dict.get id toIdDict


empty : Layer
empty =
    Layer 0 Dict.empty Dict.empty


toList : Layer -> List G.NodeId
toList (Layer _ _ toIdDict) =
    Dict.values toIdDict


length : Layer -> Int
length (Layer size _ _) =
    size


orderDict : Layer -> Dict G.NodeId Int
orderDict (Layer _ toOrderDict _) =
    toOrderDict


append : G.NodeId -> Layer -> Layer
append id (Layer size toOrderDict toIdDict) =
    Layer
        (size + 1)
        (Dict.insert id size toOrderDict)
        (Dict.insert size id toIdDict)


sortBy : (G.NodeId -> comparable) -> Layer -> Layer
sortBy f (Layer _ _ toIdDict) =
    let
        tuples =
            Dict.values toIdDict
                |> List.map (\v -> ( v, f v ))
                |> List.sortBy Tuple.second
    in
    List.foldl (\( id, _ ) acc -> append id acc) empty tuples


swapAt : Int -> Int -> Layer -> Layer
swapAt ord1 ord2 ((Layer size toOrderDict toIdDict) as orig) =
    case Dict.get ord1 toIdDict of
        Nothing ->
            orig

        Just id1 ->
            case Dict.get ord2 toIdDict of
                Nothing ->
                    orig

                Just id2 ->
                    Layer size
                        (toOrderDict
                            |> Dict.insert id1 ord2
                            |> Dict.insert id2 ord1
                        )
                        (toIdDict
                            |> Dict.insert ord1 id2
                            |> Dict.insert ord2 id1
                        )


fromList : List G.NodeId -> Layer
fromList ids =
    List.foldl append empty ids

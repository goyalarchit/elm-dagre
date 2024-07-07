module Dagre.Position.BK exposing (NodePointDict, positionX, width)

import Dagre.Attributes as DA
import Dagre.Layer as DL
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G
import List.Extra as LE



{-
   Naming Convention
   If we get a value from dictionary "dict" for key "v",
   and store it in a variable, then the name of variable should be
   dict_v == dict[v]
   the variable holding the maybe value is named
   "dict_v_"

   If we update the key "v" in a dictionary "dict",
   then the new updated dictionary is stored in
   "dictV"

-}
{-
   This module provides coordinate assignment based on Brandes and Köpf, "Fast
   and Simple Horizontal Coordinate Assignment."
-}
{-
   The following type maps a Node Id to one of the coordinates either x or y
-}


type alias NodePointDict =
    Dict G.NodeId Float


type alias NodeDict =
    Dict G.NodeId G.NodeId


type VertDir
    = Up
    | Down


type HorizDir
    = Left
    | Right



{-
   This function is exposed to other modules and runs the Brandes Kopf, Algorithm 4
-}


positionX : DA.Config -> G.Graph n e -> ( List DL.Layer, List DU.Edge ) -> NodePointDict
positionX config g ( rankList, edges ) =
    let
        edgesWithType =
            DU.markEdgesWithEdgeType g edges

        ( type1Conflicts, _ ) =
            preprocessing ( rankList, edgesWithType )

        conflicts =
            type1Conflicts

        vhDir =
            List.map (\v -> List.map (\h -> ( v, h )) [ Left, Right ]) [ Up, Down ]
                |> List.concat

        xss =
            List.map (\d -> ( d, positionXHelper config g ( rankList, edges ) conflicts d )) vhDir

        smallestWidthAlign =
            findSmallestWidthAlignment config (List.map Tuple.second xss)

        alignedXss =
            alignCoordinates xss smallestWidthAlign
    in
    balance alignedXss



{-
   This is the implementation of Algorithm 1 from Brandes and kopf
   The following function implements the preprocessing step. The algorithm marks
   all the type1 conflicts and type2(to be implemented) conflicts.
   TODO: Type2 Conflicts marking

-}


preprocessing : ( List DL.Layer, List DU.EdgeWithType ) -> ( List DU.Edge, List DU.Edge )
preprocessing ( rankList, edges ) =
    let
        allType1Conflicts =
            findType1Conflicts ( rankList, edges )
    in
    ( allType1Conflicts, [] )



{-
   This function finds the horizontal coordinates for a given Vertical
   and Horizontal direction

-}


positionXHelper : DA.Config -> G.Graph n e -> ( List DL.Layer, List DU.Edge ) -> List DU.Edge -> ( VertDir, HorizDir ) -> NodePointDict
positionXHelper config g ( rankList, edges ) conflicts ( vDir, hDir ) =
    let
        ( intAdjustedRankList, neighbourFn ) =
            case vDir of
                Up ->
                    ( rankList, DU.alongIncomingEdges edges )

                Down ->
                    ( List.reverse rankList, DU.alongOutgoingEdges edges )

        finalAdjustedRankList =
            case hDir of
                Left ->
                    intAdjustedRankList

                Right ->
                    List.map DL.reverse intAdjustedRankList

        ( root, align ) =
            verticalAlignment finalAdjustedRankList conflicts neighbourFn

        xs =
            horizontalCompaction ( config, g ) finalAdjustedRankList root align
    in
    case hDir of
        Left ->
            xs

        Right ->
            Dict.map (\_ c -> -c) xs



{-
   This is the implementation of algorithm 2 of Brandes-Kopf
   This function groups the nodes into blocks and returns two circular
   linked lists, root and align, which is used for the actual horizontal
   Coordinate assignment.
-}


verticalAlignment : List DL.Layer -> List DU.Edge -> DU.NeighbourFn -> ( NodeDict, NodeDict )
verticalAlignment rankList conflicts neighbourFn =
    let
        root =
            Dict.fromList <| List.map (\n -> ( n, n )) (List.concatMap DL.toList rankList)

        align =
            Dict.fromList <| List.map (\n -> ( n, n )) (List.concatMap DL.toList rankList)

        pos =
            getPosDict rankList

        ( finalRoot, finalAlign ) =
            List.foldl (verticalAlignmentVisitLayer pos conflicts neighbourFn) ( root, align ) rankList
    in
    ( finalRoot, finalAlign )



{-
   This is the implementation of algorithm 3 of Brandes-Kopf
   This function performs the horizontal compaction and Coordinate assignment.
-}


horizontalCompaction : ( DA.Config, G.Graph n e ) -> List DL.Layer -> NodeDict -> NodeDict -> NodePointDict
horizontalCompaction ( config, g ) rankList root align =
    let
        sepFn =
            sep config g

        sink =
            Dict.fromList <| List.map (\n -> ( n, n )) (List.concatMap DL.toList rankList)

        shift =
            Dict.fromList <| List.map (\n -> ( n, DU.infinity )) (List.concatMap DL.toList rankList)

        pred =
            getPredDict rankList

        succ =
            getSuccessorDict rankList

        xs =
            Dict.empty

        roots =
            List.filter (\v -> Just v == Dict.get v root) (List.concatMap DL.toList rankList)

        ( updSink, updXs ) =
            List.foldl (placeBlock pred sepFn root align) ( sink, xs ) roots

        updShift =
            List.foldl (\l s -> classOffsets sepFn pred succ root align updSink updXs l s) shift rankList

        finXs =
            List.foldl (\l xs_ -> List.foldl (assignAbsoluteX updShift updSink) xs_ (DL.toList l)) updXs rankList
    in
    finXs


placeBlock : NodeDict -> (G.NodeId -> G.NodeId -> Float) -> NodeDict -> NodeDict -> G.NodeId -> ( NodeDict, NodePointDict ) -> ( NodeDict, NodePointDict )
placeBlock pred sepFn root align v ( sink, xs ) =
    case Dict.get v xs of
        Nothing ->
            let
                xsV =
                    Dict.insert v 0 xs

                ( updatedSink, updatedXS ) =
                    placeBlockHelper pred sepFn root align v v ( sink, xsV )

                final_sink_v =
                    Dict.get v updatedSink
                        |> Maybe.withDefault v

                final_x_v =
                    Dict.get v updatedXS
                        |> Maybe.withDefault 0
            in
            case Dict.get v align of
                Nothing ->
                    ( updatedSink, updatedXS )

                Just w ->
                    placeAlignWholeBlock w align v final_sink_v final_x_v ( updatedSink, updatedXS )

        Just _ ->
            ( sink, xs )



{-
   assigns the absolute x coordinates
   This line differs from the source paper. See
   http://www.inf.uni-konstanz.de/~brandes/publications/ for details.
-}


assignAbsoluteX : NodePointDict -> NodeDict -> G.NodeId -> NodePointDict -> NodePointDict
assignAbsoluteX shift sink v xs =
    let
        shift_sink_v_ =
            Dict.get v sink |> Maybe.andThen (\sv -> Dict.get sv shift) |> Maybe.withDefault 0
    in
    Dict.update v (Maybe.map (\x -> x + shift_sink_v_)) xs


sep : DA.Config -> G.Graph n e -> (G.NodeId -> G.NodeId -> Float)
sep config g =
    let
        initDummyId =
            case G.nodeIdRange g of
                Nothing ->
                    0

                Just ( _, maxNodeId ) ->
                    maxNodeId + 1

        getSep =
            \nId ->
                if DU.isDummyNode initDummyId nId then
                    config.edgeSep

                else
                    config.nodeSep

        getWidth =
            \n -> width config n
    in
    \u v ->
        (getWidth u + getSep u + getSep v + getWidth v) / 2



{-
   Returns the alignment with the smallest width
-}


findSmallestWidthAlignment : DA.Config -> List NodePointDict -> NodePointDict
findSmallestWidthAlignment config xss =
    let
        getWidth =
            \n -> width config n

        minX =
            \xs -> Dict.map (\k v -> v - (getWidth k / 2)) xs |> Dict.values |> List.minimum |> Maybe.withDefault 0

        maxX =
            \xs -> Dict.map (\k v -> v + (getWidth k / 2)) xs |> Dict.values |> List.maximum |> Maybe.withDefault 0

        widthXss =
            List.map (\xs -> ( maxX xs - minX xs, xs )) xss

        defXs =
            Maybe.withDefault ( 0, Dict.empty ) (LE.getAt 0 widthXss)
    in
    LE.minimumBy Tuple.first widthXss
        |> Maybe.withDefault defXs
        |> Tuple.second



{-
   Align the coordinates of each of the layout alignments such that
   left-biased alignments have their minimum coordinate at the same point as
   the minimum coordinate of the smallest width alignment and right-biased
   alignments have their maximum coordinate at the same point as the maximum
   coordinate of the smallest width alignment.
-}


alignCoordinates : List ( ( VertDir, HorizDir ), NodePointDict ) -> NodePointDict -> List NodePointDict
alignCoordinates xss alignTo =
    let
        minX =
            \xs -> Dict.values xs |> List.minimum |> Maybe.withDefault 0

        maxX =
            \xs -> Dict.values xs |> List.maximum |> Maybe.withDefault 0

        alignToMin =
            minX alignTo

        alignToMax =
            maxX alignTo

        delta =
            \hDir xs ->
                case hDir of
                    Left ->
                        alignToMin - minX xs

                    Right ->
                        alignToMax - maxX xs

        deltas =
            List.map (\( ( _, hDir ), xs ) -> delta hDir xs) xss

        xss_ =
            List.map2 (\( ( _, _ ), xs ) del -> Dict.map (\_ x -> x + del) xs) xss deltas
    in
    xss_



{-
   Balances the layout by taking the Average Median of coordinates based on
   different biases.
-}


balance : List NodePointDict -> NodePointDict
balance xss =
    let
        helper =
            \n x xsC ->
                if Dict.member n xsC then
                    Dict.update n (Maybe.map (\xCoords -> x :: xCoords)) xsC

                else
                    Dict.insert n [ x ] xsC

        appendXs =
            \xs xsC -> Dict.foldl helper xsC xs

        multiXs =
            List.foldl appendXs Dict.empty xss

        sortedMultiXs =
            Dict.map (\_ x -> List.sort x) multiXs

        finalX =
            \l ->
                case l of
                    [ _, x1, x2, _ ] ->
                        (x1 + x2) / 2

                    [ _, x, _ ] ->
                        x

                    [ x0, x1 ] ->
                        (x0 + x1) / 2

                    [ x ] ->
                        x

                    [] ->
                        0

                    x :: _ ->
                        x
    in
    Dict.map (\_ xList -> finalX xList) sortedMultiXs



{-
   Following are helper functions for Preprocessing Step (Algorithm 1 from BK)

-}


findType1Conflicts : ( List DL.Layer, List DU.EdgeWithType ) -> List DU.Edge
findType1Conflicts ( rankList, edges ) =
    let
        adjacentLayers =
            DU.getAdjacentLayerPairs rankList
    in
    List.concat <| List.map (type1VisitLayer edges) adjacentLayers



{-
   map all the nodes in adjacent layer to their order, and the edges to orders too


   after marking one layer's conflicting edges remap them to vertices

-}


type1VisitLayer : List DU.EdgeWithType -> ( DL.Layer, DL.Layer ) -> List DU.Edge
type1VisitLayer edges ( l1, l2 ) =
    let
        reqEdges =
            DU.getEdgesWithTypeDirectedFromLayers ( l1, l2 ) edges
                |> List.map (DU.mapEdgeWithTypeToOrder ( l1, l2 ))

        prevLayerLength =
            DL.length l1

        layerLength =
            DL.length l2

        rawType1Conflicts =
            List.foldl (findInnerSegmentAndMarkConflicts ( prevLayerLength, layerLength ) reqEdges) ( ( 0, 0 ), [] ) (List.range 0 (layerLength - 1))
                |> Tuple.second

        type1Conflicts =
            List.map (DU.mapEdgeOrderToNode ( l1, l2 )) rawType1Conflicts
    in
    type1Conflicts


findInnerSegmentAndMarkConflicts : ( Int, Int ) -> List DU.EdgeWithType -> Int -> ( ( Int, Int ), List DU.Edge ) -> ( ( Int, Int ), List DU.Edge )
findInnerSegmentAndMarkConflicts ( prevLayerLength, layerLength ) edges l1 ( ( k0, scanPos ), type1Conflicts ) =
    let
        w =
            findOtherInnerSegmentNode edges l1
    in
    case ( w, l1 == layerLength - 1 ) of
        ( Nothing, False ) ->
            ( ( k0, scanPos ), type1Conflicts )

        ( Just k1, _ ) ->
            let
                subLayer =
                    List.range scanPos l1

                newConflictsList =
                    List.map (markType1Conflicts edges ( k0, k1 )) subLayer

                newConflicts =
                    List.concat newConflictsList
            in
            ( ( k1, l1 + 1 ), List.append type1Conflicts newConflicts )

        ( Nothing, True ) ->
            let
                k1 =
                    prevLayerLength - 1

                subLayer =
                    List.range scanPos l1

                newConflictsList =
                    List.map (markType1Conflicts edges ( k0, k1 )) subLayer

                newConflicts =
                    List.concat newConflictsList
            in
            ( ( k1, l1 + 1 ), List.append type1Conflicts newConflicts )


findOtherInnerSegmentNode : List DU.EdgeWithType -> Int -> Maybe Int
findOtherInnerSegmentNode edges nodeId =
    let
        innerEdges =
            DU.getInEdges nodeId edges |> DU.filterEdgesByType DU.Inner

        upperNodeOfInnerSegments =
            List.map Tuple.first innerEdges
    in
    List.minimum upperNodeOfInnerSegments


markType1Conflicts : List DU.EdgeWithType -> ( Int, Int ) -> Int -> List DU.Edge
markType1Conflicts edges ( k0, k1 ) l =
    let
        nonInnerEdges =
            DU.getInEdges l edges |> DU.filterEdgesByType DU.NonInner

        conflictingNonInnerEdges =
            List.filter (\( f, _ ) -> checkType1Conflict ( k0, k1 ) f) nonInnerEdges
    in
    conflictingNonInnerEdges


checkType1Conflict : ( Int, Int ) -> Int -> Bool
checkType1Conflict ( k0, k1 ) k =
    if k < k0 || k > k1 then
        True

    else
        False



{-
   The functions that start from here are helper function for
   Vertical Alignment Function
   based on Algorithm-2 of Brandes and Kopf
-}


getPosDict : List DL.Layer -> Dict G.NodeId Int
getPosDict rankList =
    rankList
        |> List.map DL.orderDict
        |> List.foldl Dict.union Dict.empty


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


hasConflict : List DU.Edge -> ( G.NodeId, G.NodeId ) -> Bool
hasConflict conflicts ( u, v ) =
    if List.member ( u, v ) conflicts || List.member ( v, u ) conflicts then
        True

    else
        False


alignVertexHelper : List DU.Edge -> G.NodeId -> ( G.NodeId, Int ) -> ( ( NodeDict, NodeDict ), Int ) -> ( ( NodeDict, NodeDict ), Int )
alignVertexHelper conflicts v ( w, pos_w ) ( ( root, align ), prevIdx ) =
    if getNode v align == v && prevIdx < pos_w && (hasConflict conflicts ( w, v ) |> not) then
        let
            updatedAlignW =
                Dict.update w (Maybe.map (\_ -> v)) align

            updatedRootV =
                Dict.update v (Maybe.map (\_ -> getNode w root)) root

            updatedAlignV =
                Dict.update v (Maybe.map (\_ -> getNode v updatedRootV)) updatedAlignW

            newPrevIdx =
                pos_w
        in
        ( ( updatedRootV, updatedAlignV ), newPrevIdx )

    else
        ( ( root, align ), prevIdx )


alignVertex : Dict G.NodeId Int -> List DU.Edge -> DU.NeighbourFn -> G.NodeId -> ( ( NodeDict, NodeDict ), Int ) -> ( ( NodeDict, NodeDict ), Int )
alignVertex pos conflicts neighbourFn v ( ( root, align ), prevIdx ) =
    let
        ws =
            neighbourFn v |> List.sortBy (getPos pos)

        mp =
            (toFloat (List.length ws) - 1) / 2

        w_mp =
            List.range (floor mp) (ceiling mp)
                |> List.map
                    (\i ->
                        case LE.getAt i ws of
                            Just w ->
                                w

                            Nothing ->
                                DU.intMin
                    )
                |> List.filter (\w -> w /= DU.intMin)
                |> List.map (\w -> ( w, getPos pos w ))

        updatedValues =
            List.foldl (alignVertexHelper conflicts v) ( ( root, align ), prevIdx ) w_mp
    in
    updatedValues


verticalAlignmentVisitLayer : Dict G.NodeId Int -> List DU.Edge -> DU.NeighbourFn -> DL.Layer -> ( NodeDict, NodeDict ) -> ( NodeDict, NodeDict )
verticalAlignmentVisitLayer pos conflicts neighbourFn layer ( root, align ) =
    let
        ( ( finalRoot, finalAlign ), _ ) =
            List.foldl (alignVertex pos conflicts neighbourFn) ( ( root, align ), -1 ) (DL.toList layer)
    in
    ( finalRoot, finalAlign )



{-
   The following functions are Helper Functions for Algorithm 3 of Brandes-Kopf
-}


getPredDictHelper : DL.Layer -> NodeDict -> NodeDict
getPredDictHelper layer pred =
    let
        layerList =
            DL.toList layer

        predecessors =
            List.take (DL.length layer - 1) layerList

        nodes =
            List.drop 1 layerList

        nodesWithPreds =
            List.map2 Tuple.pair nodes predecessors

        finalDict =
            List.foldl (\( n, p ) predDict -> Dict.insert n p predDict) pred nodesWithPreds
    in
    finalDict


getPredDict : List DL.Layer -> NodeDict
getPredDict rankList =
    let
        initDict =
            Dict.empty

        pred =
            List.foldl getPredDictHelper initDict rankList
    in
    pred


getSuccessorDictHelper : DL.Layer -> NodeDict -> NodeDict
getSuccessorDictHelper layer succ =
    let
        layerList =
            DL.toList layer

        successors =
            List.drop 1 layerList

        nodes =
            List.take (DL.length layer - 1) layerList

        nodesWithSuccs =
            List.map2 Tuple.pair nodes successors

        finalDict =
            List.foldl (\( n, s ) succDict -> Dict.insert n s succDict) succ nodesWithSuccs
    in
    finalDict


getSuccessorDict : List DL.Layer -> NodeDict
getSuccessorDict rankList =
    let
        initDict =
            Dict.empty

        succ =
            List.foldl getSuccessorDictHelper initDict rankList
    in
    succ


getNodeFromDict : G.NodeId -> NodeDict -> G.NodeId
getNodeFromDict node dict =
    case Dict.get node dict of
        Just x ->
            x

        Nothing ->
            node


classOffsets : (G.NodeId -> G.NodeId -> Float) -> NodeDict -> NodeDict -> NodeDict -> NodeDict -> NodeDict -> NodePointDict -> DL.Layer -> NodePointDict -> NodePointDict
classOffsets sepFn pred succ root align sink xs layer shift =
    case DL.toId 0 layer of
        Just v ->
            case Dict.get v sink of
                Nothing ->
                    shift

                Just s_v ->
                    if s_v == v then
                        let
                            updatedShift =
                                if Dict.get v shift == Just DU.infinity then
                                    Dict.update v (\_ -> Just 0) shift

                                else
                                    shift
                        in
                        classOffsetsHelper v sepFn pred succ root align sink xs updatedShift

                    else
                        shift

        Nothing ->
            shift


classOffsetsHelper : G.NodeId -> (G.NodeId -> G.NodeId -> Float) -> NodeDict -> NodeDict -> NodeDict -> NodeDict -> NodeDict -> NodePointDict -> NodePointDict -> NodePointDict
classOffsetsHelper v sepFn pred succ root align sink xs shift =
    if getNodeFromDict v align /= getNodeFromDict v root then
        let
            v_new =
                getNodeFromDict v align

            updatedShift =
                case Dict.get v_new pred of
                    Nothing ->
                        shift

                    Just u ->
                        let
                            delta =
                                sepFn u v_new

                            sink_v_ =
                                Dict.get v_new sink

                            sink_u_ =
                                Dict.get u sink

                            xs_v_ =
                                Dict.get v_new xs

                            xs_u_ =
                                Dict.get u xs

                            shift_sink_v_ =
                                Maybe.andThen (\sink_v -> Dict.get sink_v shift) sink_v_

                            shift_sink_u_ =
                                Maybe.andThen (\sink_u -> Dict.get sink_u shift) sink_u_

                            updateValue =
                                case ( ( shift_sink_u_, shift_sink_v_ ), ( xs_v_, xs_u_ ) ) of
                                    ( ( Just ss_u, Just ss_v ), ( Just xs_v, Just xs_u ) ) ->
                                        Just (min ss_u (ss_v + xs_v - xs_u - delta))

                                    ( ( Nothing, Just ss_v ), ( Just xs_v, Just xs_u ) ) ->
                                        Just (ss_v + xs_v - xs_u - delta)

                                    _ ->
                                        Nothing

                            updShift =
                                case sink_u_ of
                                    Just sink_u ->
                                        Dict.update sink_u (\_ -> updateValue) shift

                                    _ ->
                                        shift
                        in
                        updShift
        in
        classOffsetsHelper v_new sepFn pred succ root align sink xs updatedShift

    else
        case Dict.get v succ of
            Nothing ->
                shift

            Just w ->
                if getNodeFromDict v sink == getNodeFromDict w sink then
                    classOffsetsHelper w sepFn pred succ root align sink xs shift

                else
                    shift


placePredecessor : G.NodeId -> NodeDict -> (G.NodeId -> G.NodeId -> Float) -> NodeDict -> NodeDict -> G.NodeId -> G.NodeId -> ( NodeDict, NodePointDict ) -> ( NodeDict, NodePointDict )
placePredecessor p pred sepFn root align v w ( sink, xs ) =
    let
        root_p_ =
            Dict.get p root

        ( pred_sink, pred_xs ) =
            case root_p_ of
                Nothing ->
                    ( sink, xs )

                Just u ->
                    placeBlock pred sepFn root align u ( sink, xs )

        updatedSink =
            if Dict.get v pred_sink == Just v then
                Dict.update v (\_ -> Dict.get (getNode p root) pred_sink) pred_sink

            else
                pred_sink

        delta =
            sepFn w p

        updatedXS =
            case root_p_ of
                Nothing ->
                    pred_xs

                Just u ->
                    if Dict.get v updatedSink == Dict.get u updatedSink then
                        let
                            xs_v_ =
                                Dict.get v pred_xs

                            xs_u_ =
                                Dict.get u pred_xs

                            updateValue =
                                case ( xs_v_, xs_u_ ) of
                                    ( Just xs_v, Just xs_u ) ->
                                        Just (max xs_v (xs_u + delta))

                                    _ ->
                                        Nothing
                        in
                        Dict.update v (\_ -> updateValue) pred_xs

                    else
                        pred_xs
    in
    ( updatedSink, updatedXS )


placeBlockHelper : NodeDict -> (G.NodeId -> G.NodeId -> Float) -> NodeDict -> NodeDict -> G.NodeId -> G.NodeId -> ( NodeDict, NodePointDict ) -> ( NodeDict, NodePointDict )
placeBlockHelper pred sepFn root align v w ( sink, xs ) =
    let
        ( final_sink, final_xs ) =
            case Dict.get w pred of
                Nothing ->
                    ( sink, xs )

                Just p ->
                    placePredecessor p pred sepFn root align v w ( sink, xs )

        w_new_ =
            Dict.get w align
    in
    if w_new_ == Just v then
        ( final_sink, final_xs )

    else
        case w_new_ of
            Nothing ->
                ( final_sink, final_xs )

            Just w_new ->
                placeBlockHelper pred sepFn root align v w_new ( final_sink, final_xs )


placeAlignWholeBlock : G.NodeId -> NodeDict -> G.NodeId -> G.NodeId -> Float -> ( NodeDict, NodePointDict ) -> ( NodeDict, NodePointDict )
placeAlignWholeBlock w align v sink_v x_v ( sink, xs ) =
    let
        updatedXS =
            Dict.update w (always <| Just x_v) xs

        updatedSink =
            Dict.update w (always <| Just sink_v) sink
    in
    case Dict.get w align of
        Nothing ->
            ( updatedSink, updatedXS )

        Just w_new ->
            if w_new /= v then
                placeAlignWholeBlock w_new align v sink_v x_v ( updatedSink, updatedXS )

            else
                ( updatedSink, updatedXS )


width : DA.Config -> (G.NodeId -> Float)
width config =
    \nodeId ->
        Dict.get nodeId config.widthDict
            |> Maybe.withDefault config.width

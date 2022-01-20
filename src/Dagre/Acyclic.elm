module Dagre.Acyclic exposing (classify, run, undo)

import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G
import IntDict



{-
   This function takes any graph and makes it acyclic.
   It also returns the reversed edges, along with new graph in
   both types i.e. G.Graph and G.AcyclicGraph
-}


run : G.Graph n e -> ( G.Graph n e, G.AcyclicGraph n e, List DU.Edge )
run graph =
    makeAcyclic ( graph, [] )


makeAcyclic : ( G.Graph n e, List DU.Edge ) -> ( G.Graph n e, G.AcyclicGraph n e, List DU.Edge )
makeAcyclic ( graph, reversedEdges ) =
    case G.checkAcyclic graph of
        Ok g ->
            ( graph, g, reversedEdges )

        Err e ->
            let
                newGraph =
                    updateEdge e graph

                errorEdge =
                    ( e.to, e.from )
            in
            makeAcyclic ( newGraph, errorEdge :: reversedEdges )


updateEdge : G.Edge e -> G.Graph n e -> G.Graph n e
updateEdge e graph =
    let
        reverse =
            \nodeCtx ->
                { nodeCtx
                    | incoming = IntDict.insert e.to e.label nodeCtx.incoming
                    , outgoing = IntDict.remove e.to nodeCtx.outgoing
                }

        remove =
            \nodeCtx ->
                { nodeCtx
                    | incoming = IntDict.remove e.from nodeCtx.incoming
                    , outgoing = IntDict.remove e.to nodeCtx.outgoing
                }
    in
    if e.from == e.to then
        -- Remove Self loop
        G.update e.from (Maybe.map remove) graph

    else
        -- Reverse Cyclic Edge
        G.update e.from (Maybe.map reverse) graph



{-
   This function reverses the reversedEdges and also reverses the ControlPoints Dictionary Values
-}


undo : List DU.Edge -> List DU.Edge -> Dict DU.Edge ( List G.NodeId, List G.NodeId ) -> Dict DU.Edge (List G.NodeId)
undo uniDirEdges biDirEdges controlPoints =
    let
        undoUniCP =
            List.foldl undoUniDir controlPoints uniDirEdges

        undoBiCP =
            List.foldl undoBiDir undoUniCP biDirEdges
    in
    Dict.map (\_ ( pts, _ ) -> pts) undoBiCP


undoUniDir : DU.Edge -> Dict DU.Edge ( List G.NodeId, List G.NodeId ) -> Dict DU.Edge ( List G.NodeId, List G.NodeId )
undoUniDir e controlPoints =
    let
        ( from, to ) =
            e

        controlPoints_e =
            Dict.get e controlPoints |> uniDirCtrlPts
    in
    Dict.remove ( from, to ) controlPoints
        |> Dict.insert ( to, from ) controlPoints_e


uniDirCtrlPts : Maybe ( List G.NodeId, List G.NodeId ) -> ( List G.NodeId, List G.NodeId )
uniDirCtrlPts ctrlPtsTpl =
    case ctrlPtsTpl of
        Nothing ->
            ( [], [] )

        Just ( [], [] ) ->
            ( [], [] )

        Just ( pts, [] ) ->
            ( List.reverse pts, [] )

        Just ( [], pts ) ->
            ( List.reverse pts, [] )

        Just ( pts1, _ ) ->
            ( List.reverse pts1, [] )


undoBiDir : DU.Edge -> Dict DU.Edge ( List G.NodeId, List G.NodeId ) -> Dict DU.Edge ( List G.NodeId, List G.NodeId )
undoBiDir e controlPoints =
    let
        ( from, to ) =
            e

        controlPoints_e =
            Dict.get e controlPoints |> biDirCtrlPts
    in
    -- if List.member ( from, to ) biDirEdges then
    --     -- if the current reversed edge is a part of original graph (bidirectional edge)
    --     -- then don't remove reversed edge, just add a new entry
    Dict.insert ( to, from ) controlPoints_e controlPoints


biDirCtrlPts : Maybe ( List G.NodeId, List G.NodeId ) -> ( List G.NodeId, List G.NodeId )
biDirCtrlPts ctrlPtsTpl =
    case ctrlPtsTpl of
        Nothing ->
            ( [], [] )

        Just ( [], [] ) ->
            ( [], [] )

        Just ( _, [] ) ->
            ( [], [] )

        Just ( [], pts ) ->
            ( List.reverse pts, [] )

        Just ( pts1, pts2 ) ->
            ( List.reverse pts2, pts1 )


{-| This function takes a list of edges (reversed Edges)
and classifies them based on the original edges into following three categories

1.  unidirectional edges
2.  bidirectional edges
3.  self loops

This function returns them as list of edges respectively

-}
classify : List DU.Edge -> List DU.Edge -> ( List DU.Edge, List DU.Edge, List DU.Edge )
classify reversedEdges originalEdges =
    List.foldl (classifier originalEdges) ( [], [], [] ) reversedEdges


{-| Helper function for classify that classifies the edges
-}
classifier : List DU.Edge -> DU.Edge -> ( List DU.Edge, List DU.Edge, List DU.Edge ) -> ( List DU.Edge, List DU.Edge, List DU.Edge )
classifier originalEdges e ( uniDirEdges, biDirEdges, selfLoops ) =
    let
        ( from, to ) =
            e
    in
    if from == to then
        ( uniDirEdges, biDirEdges, e :: selfLoops )

    else if List.member e originalEdges then
        ( uniDirEdges, e :: biDirEdges, selfLoops )

    else
        ( e :: uniDirEdges, biDirEdges, selfLoops )

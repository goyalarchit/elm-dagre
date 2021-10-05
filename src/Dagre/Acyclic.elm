module Dagre.Acyclic exposing (..)

import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G
import IntDict



{-
   This function takes any graph and makes it acyclic.
   It also returns the reveresed edges, along with new graph in
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
                    reverseEdge e graph

                errorEdge =
                    ( e.to, e.from )
            in
            makeAcyclic ( newGraph, errorEdge :: reversedEdges )


reverseEdge : G.Edge e -> G.Graph n e -> G.Graph n e
reverseEdge e graph =
    let
        reverse =
            \nodeCtx ->
                { nodeCtx
                    | incoming = IntDict.insert e.to e.label nodeCtx.incoming
                    , outgoing = IntDict.remove e.to nodeCtx.outgoing
                }
    in
    G.update e.from (Maybe.map reverse) graph



{-
   This function reverses the reveresedEdges and also reverses the ControlPoints Dictionary Values
-}


undo : List DU.Edge -> List DU.Edge -> Dict DU.Edge (List G.NodeId) -> Dict DU.Edge (List G.NodeId)
undo originalEdges reveresedEdges controlPoints =
    List.foldl (undoHelper originalEdges) controlPoints reveresedEdges


undoHelper : List DU.Edge -> DU.Edge -> Dict DU.Edge (List G.NodeId) -> Dict DU.Edge (List G.NodeId)
undoHelper originalEdges e controlPoints =
    let
        ( from, to ) =
            e

        controlPoints_e =
            Maybe.withDefault [] (Dict.get e controlPoints)
    in
    -- if the current reversed edge is a part of original graph (bidirectional edge)
    -- then dont remove reversed edge, just add a new entry
    if List.member ( from, to ) originalEdges then
        Dict.insert ( to, from ) (List.reverse controlPoints_e) controlPoints
        -- else remove the reversed edge, and add a new edge (original edge) with control Points reversed.

    else
        Dict.remove ( from, to ) controlPoints
            |> Dict.insert ( to, from ) (List.reverse controlPoints_e)

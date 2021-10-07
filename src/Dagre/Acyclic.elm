module Dagre.Acyclic exposing (run, undo)

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
    if List.member ( from, to ) originalEdges then
        -- if the current reversed edge is a part of original graph (bidirectional edge)
        -- then dont remove reversed edge, just add a new entry
        Dict.insert ( to, from ) (List.reverse controlPoints_e) controlPoints

    else
        -- else remove the reversed edge, and add a new edge (original edge) with control Points reversed.
        Dict.remove ( from, to ) controlPoints
            |> Dict.insert ( to, from ) (List.reverse controlPoints_e)

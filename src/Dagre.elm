module Dagre exposing
    ( GraphLayout
    , runLayout
    , defaultConfig
    )

{-| This module is the core module that implements the sugiyama style graph
drawing based on popular js library [dagrejs](https://github.com/dagrejs/dagre).

This module assumes graphs are defined using elm-community/graph module.


# Return Types

@docs GraphLayout


# API

@docs runLayout


# default configurations

@docs defaultConfig

-}

import Dagre.Acyclic as DAC
import Dagre.Attributes as DA
import Dagre.Normalize as DN
import Dagre.Order as DO
import Dagre.Position as DP
import Dagre.Rank as DR
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G


{-| This type represents the record returned by the runLayout function.

1.  The width and height fields represent the width and height of graph.
2.  The coordDict contains the dictionary that maps node-ids to the coordinates
    on cartesian plane.
3.  The controlPtsDict contains a dictionary that maps edges to its control/bend points
    (list of node-ids)

-}
type alias GraphLayout =
    { width : Float
    , height : Float
    , coordDict : Dict G.NodeId DU.Coordinates
    , controlPtsDict : Dict DU.Edge (List G.NodeId)
    }


{-| This represents the default configuration of runLayout function.
For more details about configuring a layout please see Dagre.Attributes
-}
defaultConfig : DA.Config
defaultConfig =
    { rankDir = DA.TB
    , widthDict = Dict.empty
    , heightDict = Dict.empty
    , width = 32
    , height = 32
    , nodeSep = 50
    , edgeSep = 10
    , rankSep = 75
    , marginX = 20
    , marginY = 20
    }



{-
   Tags used in comments (relevant for developers working on this project)
   - TODO : need to implement this
   - Possible Future Bug : A code that might cause a bug in future,
                           discovered when some user uses it for some
                           corner case. (this need not be tested for now)
-}
{-
   main function that returns the Dagre Layout
   Positions and Control Points for splines.
   This package implements Sugiyama style graph drawing.
   The Sugiyama framework has the following phases in order of execution
   1. Making Graph Acyclic (Removing cycles)
   2. Rank/Layer Assignment
   3. Normalizing (Removing Long Edges)
   4. Vertex Ordering (Reducing Number of Edge Crossing)
   5. Coordinate Assignment
-}


{-| This is the main function that computes the layout for a graph using sugiyama
style graph drawing.

This function takes a list of Dagre Attributes and a graph and outputs the layout.

    -- simpleGraph = Graph from Readme (or any graph)
    runLayout [] simpleGraph

All the computed coordinates lie between (0,0) and (width,height).
In simple terms, you can set `viewBox 0 0 width height` in the svg.

-}
runLayout : List DA.Attribute -> G.Graph n e -> GraphLayout
runLayout edits graph =
    let
        config =
            List.foldl (\f a -> f a) defaultConfig edits

        ( newGraph, newAcyclicGraph, reversedEdges ) =
            DAC.run graph

        edges =
            DU.getEdges newGraph

        rankList =
            DR.assignRanks newAcyclicGraph

        ( ( newRankList, newEdges ), controlPoints ) =
            DN.addDummyNodesAndSplitEdges ( rankList, edges )

        bestRankList =
            DO.vertexOrder ( newRankList, newEdges )

        ( finalDict, ( w, h ) ) =
            DP.position config newGraph ( bestRankList, newEdges )

        finalControlPoints =
            DAC.undo (DU.getEdges graph) reversedEdges controlPoints
    in
    { width = w
    , height = h
    , coordDict = finalDict
    , controlPtsDict = finalControlPoints
    }

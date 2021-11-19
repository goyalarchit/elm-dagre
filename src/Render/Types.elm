module Render.Types exposing
    ( NodeAttributes, EdgeAttributes
    , NodeDrawer, EdgeDrawer
    )

{-| This module provides the type definitions used in the Render module.


# Data

The nodes and edge data available for writing a drawer.

@docs NodeAttributes, EdgeAttributes


# Drawers

These types are used by the draw function to build the actual svg.
You can use these types to write custom drawers. For inspiration you can
view the source file for Render.StandardDrawers

@docs NodeDrawer, EdgeDrawer

-}

import Graph exposing (Edge, Node)
import TypedSvg.Core exposing (Svg)


{-| This type represents all data available for rendering a node
-}
type alias NodeAttributes n =
    { node : Node n
    , coord : ( Float, Float )
    , width : Float
    , height : Float
    }


{-| This type represents all data available for rendering an edge
-}
type alias EdgeAttributes e =
    { edge : Edge e
    , source : ( Float, Float )
    , target : ( Float, Float )
    , controlPts : List ( Float, Float )
    , sourceDimensions : ( Float, Float )
    , targetDimensions : ( Float, Float )
    }


{-| This type represents a function that translates NodeAttributes to Svg
You can use this type definition to write custom node drawers.
-}
type alias NodeDrawer n msg =
    NodeAttributes n -> Svg msg


{-| This type represents a function that translates EdgeAttributes to Svg
You can use this type definition to write custom edge drawers.
-}
type alias EdgeDrawer e msg =
    EdgeAttributes e -> Svg msg

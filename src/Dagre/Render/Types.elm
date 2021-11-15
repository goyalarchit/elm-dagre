module Dagre.Render.Types exposing (..)

import Graph exposing (Edge, Node)
import TypedSvg.Core exposing (Svg)


{-| -}
type alias DrawConfig n e msg =
    { edgeDrawer : EdgeDrawer e msg
    , nodeDrawer : NodeDrawer n msg
    , style : () -> String
    }


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
    }


{-| This type represents a function that translates NodeAttributes to Svg
-}
type alias NodeDrawer n msg =
    NodeAttributes n -> Svg msg


{-| This type represents a function that translates EdgeAttributes to Svg
-}
type alias EdgeDrawer e msg =
    EdgeAttributes e -> Svg msg

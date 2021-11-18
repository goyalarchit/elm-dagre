module Render.StandardDrawers.ConfigTypes exposing (..)

{-| This module exposes the Drawer configuration Types.


# Types

@docs EdgeDrawerConfig, NodeDrawerConfig

-}

import Color exposing (Color)
import Graph exposing (Edge, Node)
import Render.StandardDrawers.Types exposing (..)


{-| This type represents all the attributes configurable for the
standard Edge Drawer
-}
type alias EdgeDrawerConfig e msg =
    { label : Edge e -> String
    , arrowHead : ArrowHeadShape
    , onClick : Maybe (Edge e -> msg)
    , strokeColor : Edge e -> Color
    , strokeWidth : Edge e -> Float
    , strokeDashArray : Edge e -> String
    , style : Edge e -> String
    , title : Edge e -> String
    , linkStyle : LinkStyle
    , alpha : Float
    , orientLabelAlongEdge : Bool
    }



{- This type represents all the attributes configurable for the
   standard Node Drawer
-}


type alias NodeDrawerConfig n msg =
    { label : Node n -> String
    , shape : Shape
    , onClick : Maybe (Node n -> msg)
    , strokeColor : Node n -> Color
    , strokeWidth : Node n -> Float
    , strokeDashArray : Node n -> String
    , style : Node n -> String
    , fill : Node n -> Color
    , title : Node n -> String
    , xLabel : Node n -> String
    , xLabelPos : Node n -> Float -> Float -> ( Float, Float )
    }

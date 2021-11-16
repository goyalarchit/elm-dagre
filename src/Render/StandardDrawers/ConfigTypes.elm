module Render.StandardDrawers.ConfigTypes exposing (..)

{-| This module exposes the Drawer configuration Types.


# Types

@docs EdgeDrawerConfig, NodeDrawerConfig

-}

import Graph exposing (Edge, Node)
import Render.StandardDrawers.Types exposing (..)
import TypedSvg.Types exposing (Length(..), Paint(..))


{-| This type represents all the attributes configurable for the
standard Edge Drawer
-}
type alias EdgeDrawerConfig e msg =
    { label : Edge e -> String
    , arrowHead : ArrowHeadShape
    , onClick : Maybe (Edge e -> msg)
    , strokeColor : Edge e -> Paint
    , strokeWidth : Edge e -> Length
    , strokeDashArray : Edge e -> String
    , style : Edge e -> String
    , fill : Edge e -> Paint
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
    , strokeColor : Node n -> Paint
    , strokeWidth : Node n -> Length
    , strokeDashArray : Node n -> String
    , style : Node n -> String
    , fill : Node n -> Paint
    , title : Node n -> String
    , xLabel : Node n -> String
    , xLabelPos : Node n -> Float -> Float -> ( Float, Float )
    }

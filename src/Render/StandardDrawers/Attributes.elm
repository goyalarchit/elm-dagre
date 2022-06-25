module Render.StandardDrawers.Attributes exposing
    ( Attribute
    , label, onClick, fontSize, strokeColor, strokeWidth, strokeDashArray, style, title
    , arrowHead, linkStyle, alpha, orientLabelAlongEdge
    , fill, shape, xLabels
    , pos
    )

{-| This module provides attributes for configuring draw, svgNodeDrawer
and svgEdgeDrawer given in Dagre.Render.

**Note** : This module uses elm-community/typed-svg, so don't confuse Svg
as elm/svg.


# Type

@docs Attribute


# Attributes Table

Please see the following table that maps which attributes are allowed for which drawers.

    | S.No. | Attribute Name       | Node's XLabel  | Node  | Edge  |
    |-------|----------------------|----------------|-------|-------|
    | 1     | label                | ✅             | ✅    | ✅    |
    | 2     | fontSize             | ✅             | ✅    | ✅    |
    | 3     | strokeColor          | ✅             | ✅    | ✅    |
    | 4     | strokeWidth          | ✅             | ✅    | ✅    |
    | 5     | strokeDashArray      | ✅             | ✅    | ✅    |
    | 6     | title                | ✅             | ✅    | ✅    |
    | 7     | onClick              | ❌             | ✅    | ✅    |
    | 8     | style                | ❌             | ✅    | ✅    |
    | 9     | fill                 | ✅             | ✅    | ❌    |
    | 10    | shape                | ✅             | ✅    | ❌    |
    | 11    | xLabels              | ✅             | ✅    | ❌    |
    | 12    | arrowHead            | ❌             | ❌    | ✅    |
    | 13    | linkStyle            | ❌             | ❌    | ✅    |
    | 14    | alpha                | ❌             | ❌    | ✅    |
    | 15    | orientLabelAlongEdge | ❌             | ❌    | ✅    |
    | 16    | pos                  | ✅             | ❌    | ❌    |


# Common Attributes

The following attributes can be used on Node, Node's xLabels and Edge Drawers.

@docs label, onClick, fontSize, strokeColor, strokeWidth, strokeDashArray, style, title


# EdgeDrawer Attributes

@docs arrowHead, linkStyle, alpha, orientLabelAlongEdge


# NodeDrawer Attributes

@docs fill, shape, xLabels


## xLabelDrawer Attributes

@docs pos

-}

import Color exposing (Color)
import Graph exposing (Node)
import Render.StandardDrawers.ConfigTypes exposing (..)
import Render.StandardDrawers.Types exposing (ArrowHeadShape, LinkStyle, Shape)
import Render.Types exposing (NodeAttributes)
import TypedSvg.Core exposing (Svg)


{-| Attribute type for Standard Drawers
-}
type alias Attribute c =
    c -> c


{-| The following attribute can be used to set label on node/edge/xlabel.
-}
label : (a -> String) -> Attribute { c | label : a -> String }
label f =
    \edc ->
        { edc | label = f }


{-| To add event handlers to Nodes and Edges
-}
onClick : (a -> msg) -> Attribute { c | onClick : Maybe (a -> msg) }
onClick f =
    \edc ->
        { edc | onClick = Just f }


{-| To set the font Size for label of a node/edge/xlabel
-}
fontSize : Float -> Attribute { c | fontSize : Float }
fontSize f =
    \edc ->
        { edc | fontSize = f }


{-| To set the stroke color of a node/edge/xlabel
-}
strokeColor : (a -> Color) -> Attribute { c | strokeColor : a -> Color }
strokeColor f =
    \edc ->
        { edc | strokeColor = f }


{-| To set the stroke width of a node/edge/xlabel
-}
strokeWidth : (a -> Float) -> Attribute { c | strokeWidth : a -> Float }
strokeWidth f =
    \edc ->
        { edc | strokeWidth = f }


{-| To set the stroke dash array of a node/edge/xlabel
-}
strokeDashArray : (a -> String) -> Attribute { c | strokeDashArray : a -> String }
strokeDashArray f =
    \edc ->
        { edc | strokeDashArray = f }


{-| To add any inline css to path element of the edge, or polygon of node.
-}
style : (a -> String) -> Attribute { c | style : a -> String }
style f =
    \edc ->
        { edc | style = f }


{-| To set the title (appears as a tooltip) of a node/edge/xlabel
-}
title : (a -> String) -> Attribute { c | title : a -> String }
title f =
    \edc ->
        { edc | title = f }



{- Node specific attributes -}


{-| This attributes sets the shape of Node used for drawing the node.
The possible values are Circle, Ellipse, Box, RoundedBox.
-}
shape : (Node n -> Shape) -> Attribute { c | shape : Node n -> Shape }
shape f =
    \ndc ->
        { ndc | shape = f }


{-| To add fill colour to Node and xLabel of node
-}
fill : (a -> Color) -> Attribute { c | fill : a -> Color }
fill f =
    \ndc ->
        { ndc | fill = f }


{-| Set the Extra Labels for a node.
-}
xLabels : List (NodeAttributes n -> Svg msg) -> Attribute (NodeDrawerConfig n msg)
xLabels f =
    \ndc ->
        { ndc | xLabels = f }


{-| Used to set the position of Extra Label relative to the node.
-}
pos : (Node n -> Float -> Float -> ( Float, Float )) -> Attribute (XLabelDrawerConfig n)
pos f =
    \ndc ->
        { ndc | pos = f }



{- Edge specific attributes -}


{-| This attributes sets the type of arrow head used for drawing the edge.
The possible values are None, Triangle, Vee.
-}
arrowHead : ArrowHeadShape -> Attribute (EdgeDrawerConfig e msg)
arrowHead ah =
    \edc ->
        { edc | arrowHead = ah }


{-| Set the style used for drawing links. The possible values are
polyline and splines
-}
linkStyle : LinkStyle -> Attribute (EdgeDrawerConfig e msg)
linkStyle ls =
    \edc ->
        { edc | linkStyle = ls }


{-| Set alpha used for drawing the spline using
[Catmull-Rom](https://package.elm-lang.org/packages/folkertdev/one-true-path-experiment/latest/Curve#catmullRom)

_Note_: alpha>=0.5 produces centripetal splines ,
which are useful to avoid self-intersections and overshoots.
[ref](https://w10schools.com/posts/225467_catmullRom%252Ealpha%28%29)

-}
alpha : Float -> Attribute (EdgeDrawerConfig e msg)
alpha a =
    \edc ->
        { edc | alpha = a }


{-| To set the label orientation along the curvature of edge
-}
orientLabelAlongEdge : Bool -> Attribute (EdgeDrawerConfig e msg)
orientLabelAlongEdge b =
    \edc ->
        { edc | orientLabelAlongEdge = b }

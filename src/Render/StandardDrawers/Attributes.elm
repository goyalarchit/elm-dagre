module Render.StandardDrawers.Attributes exposing (..)

{-| This module provides attributes for configuring draw, svgNodeDrawer
and svgEdgeDrawer given in Dagre.Render.

**Note** : This module uses elm-community/typed-svg, so don't confuse Svg
as elm/svg.


# Common Attributes

The following attributes can be used both on Node and Edge Drawers.

@docs fill, label, onClick, strokeColor, strokeWidth, strokeDashArray, style, title


# EdgeDrawer Attributes

@docs arrowHead, linkStyle, alpha


# NodeDrawer Attributes

@docs shape, xLabel, xLabelPos

-}

import Graph exposing (Node)
import Render.StandardDrawers.ConfigTypes exposing (..)
import Render.StandardDrawers.Types exposing (ArrowHeadShape, LinkStyle, Shape)
import TypedSvg.Types exposing (Length(..), Paint(..))


type alias Attribute c =
    c -> c


{-| The following attribute can be used to set label on both Nodes and Edges.
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


strokeColor : (a -> Paint) -> Attribute { c | strokeColor : a -> Paint }
strokeColor f =
    \edc ->
        { edc | strokeColor = f }


strokeWidth : (a -> Length) -> Attribute { c | strokeWidth : a -> Length }
strokeWidth f =
    \edc ->
        { edc | strokeWidth = f }


strokeDashArray : (a -> String) -> Attribute { c | strokeDashArray : Maybe (a -> String) }
strokeDashArray f =
    \edc ->
        { edc | strokeDashArray = Just f }


{-| To add any inline css to path element of the edge, or polygon of node.
-}
style : (a -> String) -> Attribute { c | style : Maybe (a -> String) }
style f =
    \edc ->
        { edc | style = Just f }


{-| To add fill color to Edge/Node
-}
fill : (a -> Paint) -> Attribute { c | fill : a -> Paint }
fill f =
    \edc ->
        { edc | fill = f }


title : (a -> String) -> Attribute { c | title : a -> String }
title f =
    \edc ->
        { edc | title = f }



{- Node specific attributes -}


{-| This attributes sets the type of arrow head used for drawing the edge.
The possible values are None, Triangle, Vee.
-}
shape : Shape -> Attribute (NodeDrawerConfig n msg)
shape s =
    \ndc ->
        { ndc | shape = s }


{-| Set the Extra Label for a node.
-}
xLabel : (Node n -> String) -> Attribute (NodeDrawerConfig n msg)
xLabel f =
    \ndc ->
        { ndc | xLabel = f }


{-| Used to set the position of Extra Label relative to the node.
-}
xLabelPos : (Node n -> Float -> Float -> ( Float, Float )) -> Attribute (NodeDrawerConfig n msg)
xLabelPos f =
    \ndc ->
        { ndc | xLabelPos = f }



{- Edge specific attributes -}


{-| This attributes sets the shape of Node used for drawing the node.
The possible values are Circle, Ellipse, Box, RoundedBox.
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



-- Need to look into this, lay edge labels along the curvature
-- {-| To set the label orientation along the curvature of edge
-- -}
-- orientLabelAlongEdge : Bool -> Attribute (EdgeDrawerConfig e msg)
-- orientLabelAlongEdge b =
--     \edc ->
--         { edc | orientLabelAlongEdge = b }

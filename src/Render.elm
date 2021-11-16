module Render exposing
    ( draw
    , edgeDrawer, nodeDrawer, style
    )

{-| This module provides a minimalistic general graph renderer for some use cases.
There are many possible ways of using the this package

1.  You can diretly use existing drawers as a plug and play solution.
2.  You can modify the existing drawers using Dagre.Rander.Attributes for
    changing some aesthetics.
3.  If you want more control you can write your own drawers and use it with
    draw function. For more details please view the source code for Render and Render.Internal

**Note** : This module uses elm-community/typed-svg, so don't confuse Svg as elm/svg.


# API

@docs draw


# Drawers

This type represents a function that translates NodeAttributes / EdgeAttributes
to Svg.
If the standard drawers are not fulfilling your usecase,
you can use these types to define custom drawers. For more details you can look
at Render.Types .


# Configuration Attributes

@docs edgeDrawer, nodeDrawer, style

-}

import Dagre as D
import Dagre.Attributes as DA
import Dict
import Graph as G exposing (Edge, Graph, Node)
import Html exposing (Html)
import Render.StandardDrawers as DRD exposing (..)
import Render.StandardDrawers.Attributes exposing (Attribute)
import Render.Types as DRT exposing (..)
import TypedSvg as TS exposing (g, polyline)
import TypedSvg.Attributes as TA exposing (class, points, stroke, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core as TC exposing (Svg)
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Cursor(..)
        , Display(..)
        , FontWeight(..)
        , Length(..)
        , MarkerCoordinateSystem(..)
        , Paint(..)
        , Transform(..)
        )



{- This type represents a configuration for the draw function -}


type alias DrawConfig n e msg =
    { edgeDrawer : EdgeDrawer e msg
    , nodeDrawer : NodeDrawer n msg
    , style : String
    }


nodeDrawing : Node n -> NodeDrawer n msg -> Dict.Dict G.NodeId ( Float, Float ) -> DA.Config -> TC.Svg msg
nodeDrawing node_ drawNode_ coordDict config =
    let
        pos =
            Maybe.withDefault ( -10, -10 ) (Dict.get node_.id coordDict)

        w =
            Maybe.withDefault config.width (Dict.get node_.id config.widthDict)

        h =
            Maybe.withDefault config.height (Dict.get node_.id config.heightDict)
    in
    drawNode_ (NodeAttributes node_ pos w h)


edgeDrawing : Edge e -> EdgeDrawer e msg -> Dict.Dict G.NodeId ( Float, Float ) -> Dict.Dict ( G.NodeId, G.NodeId ) (List G.NodeId) -> TC.Svg msg
edgeDrawing edge_ drawEdge_ coordDict controlPointsDict =
    let
        getCoords =
            \p -> Maybe.withDefault ( -10, -10 ) (Dict.get p coordDict)

        sourcePos =
            getCoords edge_.from

        targetPos =
            getCoords edge_.to

        ctrlPts =
            Maybe.withDefault [] (Dict.get ( edge_.from, edge_.to ) controlPointsDict) |> List.map getCoords
    in
    drawEdge_ (EdgeAttributes edge_ sourcePos targetPos ctrlPts)



{- defualt config for the draw function -}


defDrawConfig : DrawConfig n e msg
defDrawConfig =
    { edgeDrawer = DRD.svgDrawEdge []
    , nodeDrawer = DRD.svgDrawNode []
    , style = ""
    }


{-| This function is draws a graph as a SVG using the Elm-Dagre module.
The first argument takes a list of Dagre attributes, and the second
argument sets the drawers and styles. The StandardDrawers are used as the
default drawers.

    -- The simplest usage is
    draw [] [] sampleGraph

You can configure the standard drawers. Please see Render.StandardDrawers for
more information.

You can also use a custom drawer by setting draw function attributes. See the
next subsection for more information.

-- Example of custom Drawer

-}
draw : List DA.Attribute -> List (Attribute (DrawConfig n e msg)) -> Graph n e -> Html msg
draw edits1 edits2 graph =
    let
        { width, height, coordDict, controlPtsDict } =
            D.runLayout edits1 graph

        dagreConfig =
            List.foldl (\f a -> f a) D.defaultConfig edits1

        drawConfig =
            List.foldl (\f a -> f a) defDrawConfig edits2

        edgesSvg =
            g [ class [ "links" ] ] <| List.map (\e -> edgeDrawing e drawConfig.edgeDrawer coordDict controlPtsDict) <| G.edges graph

        nodesSvg =
            g [ class [ "nodes" ] ] <| List.map (\n -> nodeDrawing n drawConfig.nodeDrawer coordDict dagreConfig) <| G.nodes graph
    in
    TS.svg
        [ TA.viewBox 0 0 width height
        , TA.style drawConfig.style
        ]
        [ TS.defs [] [ triangleHeadElement, veeHeadElement ]
        , g [ TA.id "graph0" ] [ edgesSvg, nodesSvg ]
        ]


{-| This attribute sets the edge drawer for draw function
Update the standard drawer configs using this attribute
-}
edgeDrawer : EdgeDrawer e msg -> Attribute (DrawConfig n e msg)
edgeDrawer f =
    \dc ->
        { dc | edgeDrawer = f }


{-| This attribute sets the node drawer for draw function
Update the drawer config using this attribute
-}
nodeDrawer : NodeDrawer n msg -> Attribute (DrawConfig n e msg)
nodeDrawer f =
    \dc ->
        { dc | nodeDrawer = f }


{-| To set inline css style for the generated graph SVG
-}
style : String -> Attribute (DrawConfig n e msg)
style s =
    \dc ->
        { dc | style = s }



{- Different svg elements for different heads on edges -}


triangleHeadElement : Svg msg
triangleHeadElement =
    TS.marker
        [ TA.id "triangle-head"
        , TA.viewBox 0 0 9 6
        , TA.markerWidth <| Px 4
        , TA.markerHeight <| Px 4
        , TA.refX "16"
        , TA.refY "3"
        , TA.orient "auto"
        , TA.markerUnits MarkerCoordinateSystemStrokeWidth
        ]
        [ TS.path
            [ TA.d "M0,0 L0,6 L9,3 z"
            , TA.stroke ContextStroke
            ]
            []
        ]


veeHeadElement : Svg msg
veeHeadElement =
    TS.marker
        [ TA.id "vee-head"
        , TA.markerWidth <| Px 10
        , TA.markerHeight <| Px 10
        , TA.refX "16"
        , TA.refY "3"
        , TA.orient "auto"
        , TA.markerUnits MarkerCoordinateSystemStrokeWidth
        ]
        [ TS.path
            [ TA.d "M0,0 L4.5,3 L0,6 L9,3 z"
            , TA.fill ContextFill
            , TA.stroke ContextStroke
            ]
            []
        ]



{-
   Return a record of following type
   1.  { Dict of NodeId to Coordinates
       , Dict of (NodeId,NodeId) List (nodeIds)
       , height : of graph
       , width : of graph}





-}

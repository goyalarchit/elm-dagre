module Render exposing
    ( draw
    , edgeDrawer, nodeDrawer, id, style
    )

{-| This module provides a minimalistic general graph renderer for some use cases.
There are many possible ways of using the this package

1.  You can directly use existing drawers as a plug and play solution.
2.  You can modify the existing drawers using Dagre.Rander.Attributes for
    changing some aesthetics.
3.  If you want more control you can write your own drawers and use it with
    draw function. For more details please view the source code for Render and Render.StandardDrawers

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

@docs edgeDrawer, nodeDrawer, id, style

-}

import Dagre as D
import Dagre.Attributes as DA
import Dict
import Graph as G exposing (Edge, Graph, Node)
import Html exposing (Html)
import Render.StandardDrawers as RSD
import Render.StandardDrawers.Attributes exposing (Attribute)
import Render.Types exposing (..)
import TypedSvg as TS
import TypedSvg.Attributes as TA
import TypedSvg.Core as TC



{- This type represents a configuration for the draw function -}


type alias DrawConfig n e msg =
    { edgeDrawer : EdgeDrawer e msg
    , nodeDrawer : NodeDrawer n msg
    , style : String
    , id : String
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


edgeDrawing : Edge e -> EdgeDrawer e msg -> Dict.Dict G.NodeId ( Float, Float ) -> Dict.Dict ( G.NodeId, G.NodeId ) (List G.NodeId) -> DA.Config -> TC.Svg msg
edgeDrawing edge_ drawEdge_ coordDict controlPointsDict config =
    let
        getCoords =
            \p -> Maybe.withDefault ( -10, -10 ) (Dict.get p coordDict)

        sourcePos =
            getCoords edge_.from

        targetPos =
            getCoords edge_.to

        ctrlPts =
            Maybe.withDefault [] (Dict.get ( edge_.from, edge_.to ) controlPointsDict) |> List.map getCoords

        getWidth =
            \n ->
                Maybe.withDefault config.width (Dict.get n config.widthDict)

        getHeight =
            \n ->
                Maybe.withDefault config.height (Dict.get n config.heightDict)

        dimensions =
            \n -> ( getWidth n, getHeight n )
    in
    drawEdge_ (EdgeAttributes edge_ sourcePos targetPos ctrlPts (dimensions edge_.from) (dimensions edge_.to))



{- defualt config for the draw function -}


defDrawConfig : DrawConfig n e msg
defDrawConfig =
    { edgeDrawer = RSD.svgDrawEdge []
    , nodeDrawer = RSD.svgDrawNode []
    , style = ""
    , id = "graph-0"
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
            TS.g [ TA.class [ "links" ] ] <| List.map (\e -> edgeDrawing e drawConfig.edgeDrawer coordDict controlPtsDict dagreConfig) <| G.edges graph

        nodesSvg =
            TS.g [ TA.class [ "nodes" ] ] <| List.map (\n -> nodeDrawing n drawConfig.nodeDrawer coordDict dagreConfig) <| G.nodes graph
    in
    TS.svg
        [ TA.viewBox 0 0 width height
        , TA.style drawConfig.style
        ]
        [ TS.g [ TA.id drawConfig.id ] [ edgesSvg, nodesSvg ]
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


{-| Set id attribute for the svg graph
-}
id : String -> Attribute (DrawConfig n e msg)
id s =
    \dc ->
        { dc | id = s }

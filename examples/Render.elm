module Render exposing (draw, label, svgDrawEdge, svgDrawEdge2, svgDrawNode)

-- import TypedSvg.Events exposing (onClick)

import Color
import Dagre as D
import Dagre.Attributes as DA
import Dict
import Graph as G exposing (Edge, Graph, Node)
import Html exposing (Html)
import Spline as S exposing (LinkStyle(..))
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



{-
   This module uses elm-community/typed-svg, so don't confuse Svg as elm/svg.
-}


type alias Attribute c =
    c -> c


type ArrowHeadShape
    = None
    | Triangle
    | Vee


type alias EdgeDrawerConfig e msg =
    { label : G.Edge e -> String
    , arrowHead : ArrowHeadShape
    , onClick : Maybe (G.Edge e -> msg)
    , strokeColor : Paint
    , strokeWidth : Length
    , strokeDashArray : Maybe String
    , style : Maybe String
    , fill : Paint
    , title : G.Edge e -> String
    , linkStyle : LinkStyle
    , smooth : Float
    }


defEdgeDrawerConfig : EdgeDrawerConfig e msg
defEdgeDrawerConfig =
    let
        f =
            \e -> String.fromInt e.from ++ " → " ++ String.fromInt e.to
    in
    { label = f
    , arrowHead = None
    , onClick = Nothing
    , strokeColor = Paint Color.black
    , strokeWidth = Px 2
    , strokeDashArray = Nothing
    , style = Nothing
    , fill = PaintNone
    , title = f
    , linkStyle = Spline
    , smooth = 0.2
    }


label : (a -> String) -> Attribute { c | label : a -> String }
label f =
    \edc ->
        { edc | label = f }


onClick : (a -> msg) -> Attribute { c | onClick : Maybe (a -> msg) }
onClick f =
    \edc ->
        { edc | onClick = Just f }


strokeColor : Paint -> Attribute { c | strokeColor : Paint }
strokeColor p =
    \edc ->
        { edc | strokeColor = p }


strokeWidth : Length -> Attribute { c | strokeWidth : Length }
strokeWidth l =
    \edc ->
        { edc | strokeWidth = l }


strokeDashArray : String -> Attribute { c | strokeDashArray : Maybe String }
strokeDashArray s =
    \edc ->
        { edc | strokeDashArray = Just s }


{-| To add any inline css to path element of the edge
-}
style : String -> Attribute { c | style : Maybe String }
style s =
    \edc ->
        { edc | style = Just s }


fill : Paint -> Attribute { c | fill : Paint }
fill p =
    \edc ->
        { edc | fill = p }


{-| Functions for setting configuration of given svg Edge Drawer
-}
arrowHead : ArrowHeadShape -> Attribute (EdgeDrawerConfig e msg)
arrowHead ah =
    \edc ->
        { edc | arrowHead = ah }



-- , title = f
-- , linkStyle = Spline
-- , smooth = 0.2


type alias NodeDrawer n msg =
    Node n -> ( Float, Float ) -> TC.Svg msg


type alias EdgeDrawer e msg =
    Edge e -> ( Float, Float ) -> ( Float, Float ) -> List ( Float, Float ) -> TC.Svg msg


arrowHeadElement : Svg msg
arrowHeadElement =
    TS.marker
        [ TA.id "arrow-head"
        , TA.markerWidth <| Px 10
        , TA.markerHeight <| Px 10
        , TA.refX "16"
        , TA.refY "3"
        , TA.orient "auto"
        , TA.markerUnits MarkerCoordinateSystemStrokeWidth
        ]
        [ TS.path
            [ TA.d "M0,0 L0,6 L9,3 z"
            ]
            []
        ]


svgDrawEdge : EdgeDrawer () msg
svgDrawEdge _ ( sourceX, sourceY ) ( targetX, targetY ) controlPts =
    polyline
        [ TA.points <| List.concat [ [ ( sourceX, sourceY ) ], controlPts, [ ( targetX, targetY ) ] ]
        , TA.stroke <| Paint Color.black
        , TA.strokeWidth <| Px 2
        , TA.fill PaintNone
        , TA.markerEnd "url(#arrow-head)"
        ]
        []


svgDrawEdge2 : List (Attribute (EdgeDrawerConfig e msg)) -> EdgeDrawer e msg
svgDrawEdge2 edits e ( sourceX, sourceY ) ( targetX, targetY ) controlPts =
    let
        config =
            List.foldl (\f a -> f a) defEdgeDrawerConfig edits
    in
    g
        []
        [ TS.title [] [ TC.text <| config.title e ]
        , TS.path
            [ S.spline 0.2 ( sourceX, sourceY ) ( targetX, targetY ) controlPts
            , TA.stroke <| Paint Color.black
            , TA.strokeWidth <| Px 2
            , TA.fill PaintNone
            , TA.markerEnd "url(#arrow-head)"
            ]
            []
        ]


svgDrawNode : NodeDrawer Int msg
svgDrawNode node ( posX, posY ) =
    g
        []
        [ TS.circle
            [ r 16
            , TA.stroke <| Paint Color.blue
            , TA.fill <| Paint Color.white
            , cx posX
            , cy posY
            ]
            []
        , TS.text_
            [ textAnchor AnchorMiddle
            , transform [ Translate posX (posY + 5) ]
            ]
            [ TC.text (String.fromInt node.id) ]
        ]


nodeDrawing : Node n -> NodeDrawer n msg -> Dict.Dict G.NodeId ( Float, Float ) -> TC.Svg msg
nodeDrawing node_ drawNode_ coordDict =
    let
        pos =
            Maybe.withDefault ( -10, -10 ) (Dict.get node_.id coordDict)
    in
    drawNode_ node_ pos


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
    drawEdge_ edge_ sourcePos targetPos ctrlPts


getCoordDict : List DA.Attribute -> Graph n e -> ( Dict.Dict G.NodeId ( Float, Float ), Dict.Dict ( G.NodeId, G.NodeId ) (List G.NodeId) )
getCoordDict edits graph =
    D.runLayout edits graph


getCanvasSize : Dict.Dict G.NodeId ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) )
getCanvasSize coordDict =
    let
        coords =
            Dict.values coordDict

        xCoords =
            List.map (\( x, _ ) -> x) coords

        yCoords =
            List.map (\( _, y ) -> y) coords

        minX =
            Maybe.withDefault -100.0 (List.minimum xCoords) - 100

        minY =
            Maybe.withDefault -100.0 (List.minimum yCoords) - 100

        maxX =
            Maybe.withDefault 200.0 (List.maximum xCoords)

        maxY =
            Maybe.withDefault 400.0 (List.maximum yCoords)
    in
    ( ( minX, minY ), ( maxX - minX + 100, maxY - minY + 100 ) )


draw : List DA.Attribute -> NodeDrawer n msg -> EdgeDrawer e msg -> Graph n e -> Html msg
draw edits drawNode drawEdge graph =
    let
        ( coordDict, controlPointsDict ) =
            getCoordDict edits graph

        ( ( minX, minY ), ( w, h ) ) =
            getCanvasSize coordDict

        config =
            List.foldl (\f a -> f a) D.defaultConfig edits

        edgesSvg =
            g [ class [ "links" ] ] <| List.map (\e -> edgeDrawing e drawEdge coordDict controlPointsDict) <| G.edges graph

        nodesSvg =
            g [ class [ "nodes" ] ] <| List.map (\n -> nodeDrawing n drawNode coordDict) <| G.nodes graph
    in
    TS.svg
        [ --   TA.width (Px w)
          -- , TA.height (Px h)
          TA.viewBox minX minY w h

        -- , TA.display DisplayInline
        ]
        [ TS.defs [] [ arrowHeadElement ]
        , g [] [ edgesSvg, nodesSvg ]
        ]



{-
   R.draw
       1. Dagre.config
       2. Graph.config
       3. R.svgDrawNode  [Node.config]
       4. R.svgDrawEdge2 [Edge.Config]
       tree




-}

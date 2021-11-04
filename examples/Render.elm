module Render exposing (draw, svgDrawEdge, svgDrawEdge2, svgDrawNode)

-- import TypedSvg.Events exposing (onClick)

import Color
import Dagre as D
import Dagre.Attributes as DA
import Dict
import Graph as G exposing (Edge, Graph, Node)
import Spline as S
import TypedSvg as TS exposing (g, polyline)
import TypedSvg.Attributes as TA exposing (class, fill, points, stroke, textAnchor, transform)
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



-- type  Layout =
--     Tree TreeLayout
--     | ForceGraph  ForceLayout


type alias NodeDrawer n fmt =
    Node n -> ( Float, Float ) -> fmt


type alias EdgeDrawer e fmt =
    Edge e -> ( Float, Float ) -> ( Float, Float ) -> List ( Float, Float ) -> fmt


arrowHead : Svg msg
arrowHead =
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


svgDrawEdge : EdgeDrawer () (Svg msg)
svgDrawEdge _ ( sourceX, sourceY ) ( targetX, targetY ) controlPts =
    polyline
        [ points <| List.concat [ [ ( sourceX, sourceY ) ], controlPts, [ ( targetX, targetY ) ] ]
        , stroke <| Paint Color.black
        , TA.strokeWidth <| Px 2
        , fill PaintNone
        , TA.markerEnd "url(#arrow-head)"
        ]
        []


svgDrawEdge2 : EdgeDrawer () (Svg msg)
svgDrawEdge2 _ ( sourceX, sourceY ) ( targetX, targetY ) controlPts =
    TS.path
        [ S.spline 0.2 ( sourceX, sourceY ) ( targetX, targetY ) controlPts
        , stroke <| Paint Color.black
        , TA.strokeWidth <| Px 2
        , fill PaintNone
        , TA.markerEnd "url(#arrow-head)"
        ]
        []


svgDrawNode : NodeDrawer Int (Svg msg)
svgDrawNode node ( posX, posY ) =
    g
        []
        [ TS.circle
            [ r 16
            , stroke <| Paint Color.blue
            , fill <| Paint Color.white
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


nodeDrawing : Node n -> NodeDrawer n fmt -> Dict.Dict G.NodeId ( Float, Float ) -> fmt
nodeDrawing node_ drawNode_ coordDict =
    let
        pos =
            Maybe.withDefault ( -10, -10 ) (Dict.get node_.id coordDict)
    in
    drawNode_ node_ pos


edgeDrawing : Edge e -> EdgeDrawer e fmt -> Dict.Dict G.NodeId ( Float, Float ) -> Dict.Dict ( G.NodeId, G.NodeId ) (List G.NodeId) -> fmt
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


draw : List DA.Attribute -> NodeDrawer n (Svg msg) -> EdgeDrawer e (Svg msg) -> Graph n e -> Svg msg
draw edits drawNode drawEdge graph =
    let
        ( coordDict, controlPointsDict ) =
            getCoordDict edits graph

        ( ( minX, minY ), ( w, h ) ) =
            getCanvasSize coordDict

        edgesSvg =
            g [ class [ "links" ] ] <| List.map (\e -> edgeDrawing e drawEdge coordDict controlPointsDict) <| G.edges graph

        nodesSvg =
            g [ class [ "nodes" ] ] <| List.map (\n -> nodeDrawing n drawNode coordDict) <| G.nodes graph
    in
    TS.svg
        [ TA.width (Px w)
        , TA.height (Px h)
        , TA.viewBox minX minY w h

        -- , TA.display DisplayInline
        ]
        [ TS.defs [] [ arrowHead ]
        , g [] [ edgesSvg, nodesSvg ]
        ]

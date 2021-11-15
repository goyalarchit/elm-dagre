module Dagre.Render.Drawers exposing (..)

import Color
import Curve
import Dagre.Render.Attributes as DRA exposing (ArrowHeadShape(..), Attribute, EdgeDrawerConfig, LinkStyle(..), NodeDrawerConfig, Shape(..))
import Dagre.Render.Types exposing (..)
import Graph exposing (Edge, Node)
import SubPath as SP
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


defEdgeDrawerConfig : EdgeDrawerConfig e msg
defEdgeDrawerConfig =
    let
        f =
            \e -> String.fromInt e.from ++ " → " ++ String.fromInt e.to

        f_ =
            \_ -> ""
    in
    { label = f_
    , arrowHead = None
    , onClick = Nothing
    , strokeColor = \_ -> Paint Color.darkGrey
    , strokeWidth = \_ -> Px 3
    , strokeDashArray = f_
    , style = f_
    , fill = \_ -> PaintNone
    , title = f
    , linkStyle = Spline
    , alpha = 0.5
    }


defNodeDrawerConfig : NodeDrawerConfig n msg
defNodeDrawerConfig =
    let
        f =
            \n -> String.fromInt n.id

        f_ =
            \_ -> ""
    in
    { label = f
    , shape = Ellipse
    , onClick = Nothing
    , strokeColor = \_ -> Paint Color.darkGrey
    , strokeWidth = \_ -> Px 3
    , strokeDashArray = f_
    , style = f_
    , fill = \_ -> PaintNone
    , title = f
    , xLabel = f_
    , xLabelPos = \_ w h -> ( (w / 2) + 1, (-h / 2) - 1 )
    }


svgDrawEdge : EdgeDrawer () msg
svgDrawEdge edgeAtrib =
    let
        edge =
            edgeAtrib.edge

        ( sourceX, sourceY ) =
            edgeAtrib.source

        ( targetX, targetY ) =
            edgeAtrib.target

        controlPts =
            edgeAtrib.controlPts
    in
    polyline
        [ TA.points <| List.concat [ [ ( sourceX, sourceY ) ], controlPts, [ ( targetX, targetY ) ] ]
        , TA.stroke <| Paint Color.black
        , TA.strokeWidth <| Px 2
        , TA.fill PaintNone
        , TA.markerEnd "url(#triangle-head)"
        ]
        []


svgDrawEdge2 : List (Attribute (EdgeDrawerConfig e msg)) -> EdgeDrawer e msg
svgDrawEdge2 edits edgeAtrib =
    let
        edge =
            edgeAtrib.edge

        ( sourceX, sourceY ) =
            edgeAtrib.source

        ( targetX, targetY ) =
            edgeAtrib.target

        controlPts =
            edgeAtrib.controlPts

        config =
            List.foldl (\f a -> f a) defEdgeDrawerConfig edits

        curve =
            Curve.catmullRom config.alpha (List.concat [ [ edgeAtrib.source ], edgeAtrib.controlPts, [ edgeAtrib.target ] ])

        tolerance =
            1.0e-4

        parameterized =
            SP.arcLengthParameterized tolerance curve

        ( midX, midY ) =
            case SP.pointAlong parameterized (SP.arcLength parameterized / 2) of
                Just m ->
                    m

                Nothing ->
                    ( -10, -10 )
    in
    g
        []
        [ TS.title [] [ TC.text <| config.title edge ]
        , TS.path
            [ TA.d <| SP.toString curve

            -- [ DRI.spline 0.2 ( sourceX, sourceY ) ( targetX, targetY ) controlPts
            , TA.stroke <| config.strokeColor edge
            , TA.strokeWidth <| config.strokeWidth edge
            , TA.fill (config.fill edge)
            , TA.markerEnd "url(#vee-head)"
            ]
            []
        , TS.text_
            [ textAnchor AnchorMiddle
            , transform [ Translate midX (midY + 5) ]
            ]
            [ TC.text (config.label edge) ]
        ]


svgDrawNode : List (Attribute (NodeDrawerConfig n msg)) -> NodeDrawer n msg
svgDrawNode edits nodeAtrib =
    let
        node =
            nodeAtrib.node

        ( posX, posY ) =
            nodeAtrib.coord

        width =
            nodeAtrib.width

        height =
            nodeAtrib.height

        d =
            max width height

        config =
            List.foldl (\f a -> f a) defNodeDrawerConfig edits

        lbl =
            config.label node
    in
    g
        []
        [ TS.circle
            [ r (d / 2)
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
            [ TC.text lbl ]
        , xLabelDrawer (config.xLabel node) config.xLabelPos nodeAtrib
        ]


xLabelDrawer : String -> (Node n -> Float -> Float -> ( Float, Float )) -> NodeDrawer n msg
xLabelDrawer lbl xLabelPos nodeAtrib =
    let
        ( posX, posY ) =
            nodeAtrib.coord

        ( xPosX, xPosY ) =
            xLabelPos nodeAtrib.node nodeAtrib.width nodeAtrib.height

        ( xlPosX, xlPosY ) =
            ( posX + xPosX, posY + xPosY )
    in
    TS.text_
        [ textAnchor AnchorMiddle
        , transform [ Translate xlPosX xlPosY ]
        ]
        [ TC.text lbl
        ]

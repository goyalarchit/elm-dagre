module Render.StandardDrawers exposing (..)

{-| This module provides the standard Drawers for drawing a graph. These are
default drawers for the draw function.


# Standard Drawers

@docs svgEdgeDrawer, svgNodeDrawer


## Standard Configurations

@docs defEdgeDrawerConfig, defNodeDrawerConfig

-}

import Bootstrap.Accordion exposing (config)
import Color
import Curve
import Graph exposing (Node)
import Render.StandardDrawers.Attributes exposing (Attribute)
import Render.StandardDrawers.ConfigTypes exposing (..)
import Render.StandardDrawers.Types exposing (..)
import Render.Types exposing (..)
import SubPath as SP
import TypedSvg as TS exposing (g)
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core as TC exposing (Svg)
import TypedSvg.Events as TE
import TypedSvg.Types as TT
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


arrowHeadId : ArrowHeadShape -> String
arrowHeadId ah =
    case ah of
        None ->
            ""

        Triangle ->
            "url(#triangle-head)"

        Vee ->
            "url(#vee-head)"


svgDrawEdge : List (Attribute (EdgeDrawerConfig e msg)) -> EdgeDrawer e msg
svgDrawEdge edits edgeAtrib =
    let
        edge =
            edgeAtrib.edge

        config =
            List.foldl (\f a -> f a) defEdgeDrawerConfig edits

        curve =
            let
                pts =
                    List.concat [ [ edgeAtrib.source ], edgeAtrib.controlPts, [ edgeAtrib.target ] ]
            in
            case config.linkStyle of
                Spline ->
                    Curve.catmullRom config.alpha pts

                Polyline ->
                    Curve.linear pts

        parameterizedCurve =
            SP.arcLengthParameterized 1.0e-4 curve

        ( midX, midY ) =
            (case SP.pointAlong parameterizedCurve (SP.arcLength parameterizedCurve / 2) of
                Just m ->
                    m

                Nothing ->
                    ( -10, -10 )
            )
                |> Tuple.mapBoth
                    (\a ->
                        if isNaN a then
                            -10

                        else
                            a
                    )
                    (\a ->
                        if isNaN a then
                            -10

                        else
                            a
                    )

        edge_id =
            "edge-" ++ String.fromInt edge.from ++ "-" ++ String.fromInt edge.to

        gAtrib =
            case config.onClick of
                Nothing ->
                    [ TA.id edge_id
                    , TA.class [ "edge" ]
                    , TA.style <| config.style edge
                    , TA.markerEnd (arrowHeadId config.arrowHead)
                    ]

                Just f ->
                    [ TA.id edge_id
                    , TA.class [ "edge" ]
                    , TA.style <| config.style edge
                    , TA.markerEnd (arrowHeadId config.arrowHead)
                    , TE.onClick (f edge)
                    ]
    in
    g
        gAtrib
        [ TS.title [] [ TC.text <| config.title edge ]
        , TS.path
            [ TA.id (edge_id ++ "-path")
            , TA.d <| SP.toString curve
            , TA.stroke <| config.strokeColor edge
            , TA.strokeWidth <| config.strokeWidth edge
            , TA.strokeDasharray <| config.strokeDashArray edge
            , TA.fill (config.fill edge)
            ]
            []
        , TS.text_
            [ TA.textAnchor AnchorMiddle
            , TA.alignmentBaseline AlignmentCentral
            , TA.transform [ Translate midX midY ]
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
            , TA.fill <| Paint (Color.rgb255 178 235 242)
            , cx posX
            , cy posY
            ]
            []
        , TS.text_
            [ TA.textAnchor AnchorMiddle
            , TA.alignmentBaseline AlignmentCentral
            , TA.transform [ Translate posX posY ]
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
        [ TA.textAnchor AnchorMiddle
        , TA.alignmentBaseline AlignmentCentral
        , TA.transform [ Translate xlPosX xlPosY ]
        ]
        [ TC.text lbl
        ]

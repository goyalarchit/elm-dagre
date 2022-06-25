module Render.StandardDrawers exposing (svgDrawEdge, svgDrawNode, svgDrawXLabel)

{-| This module provides the standard Drawers for drawing a graph. These are
default drawers for the draw function.


# Standard Drawers

@docs svgDrawEdge, svgDrawNode, svgDrawXLabel


## Standard Configurations

_Note_: All Values are in [px](https://package.elm-lang.org/packages/elm-community/typed-svg/latest/TypedSvg-Types#px)


### Edge Drawer

1.  arrowHead = None
2.  fontSize = 16
3.  strokeColor = Color.darkGrey
4.  strokeWidth = 3px
5.  title = Edge.from → Edge.to
6.  linkStyle = Spline
7.  alpha = 0.5
8.  orientLabelAlongEdge = False

_Note: All missing attributes don't have any preset values_


### NodeDrawer

1.  label = Node.id
2.  shape = Ellipse
3.  fontSize = 16
4.  strokeColor = Color.blue
5.  strokeWidth = 1px
6.  fill = #b2ebf2
7.  title = Node.id
8.  xLabels = []

_Note: All missing attributes don't have any preset values_


### xLabelDrawer

1.  pos = \_ w h -> ( (w / 2) + 1, (-h / 2) - 1 )
2.  shape = NoShape
3.  fontSize = 8
4.  strokeColor = \_ -> Color.blue
5.  strokeWidth = \_ -> 1
6.  fill = \_ -> Color.white

_Note:_

1.  _All missing attributes don't have any preset values._
2.  _No shape is drawn for xLabel by default, hence no other attributes like stroke, fill are rendered._

-}

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
import TypedSvg.Core as TC exposing (Svg)
import TypedSvg.Events as TE
import TypedSvg.Types as TT


type alias ShapeAttributes n =
    { shape : Node n -> Shape
    , strokeColor : Node n -> Color.Color
    , strokeWidth : Node n -> Float
    , strokeDashArray : Node n -> String
    , fill : Node n -> Color.Color
    }


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
    , fontSize = 16
    , strokeColor = \_ -> Color.darkGrey
    , strokeWidth = \_ -> 3
    , strokeDashArray = f_
    , style = f_
    , title = f
    , linkStyle = Spline
    , alpha = 0.5
    , orientLabelAlongEdge = False
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
    , shape = \_ -> Ellipse
    , onClick = Nothing
    , fontSize = 16
    , strokeColor = \_ -> Color.blue
    , strokeWidth = \_ -> 1
    , strokeDashArray = f_
    , style = f_
    , fill = \_ -> Color.rgb255 178 235 242
    , title = f
    , xLabels = []
    }


defXLabelDrawerConfig : XLabelDrawerConfig n
defXLabelDrawerConfig =
    let
        f_ =
            \_ -> ""
    in
    { label = f_
    , pos = \_ w h -> ( (w / 2) + 1, (-h / 2) - 1 )
    , shape = \_ -> NoShape
    , fontSize = 8
    , strokeColor = \_ -> Color.blue
    , strokeWidth = \_ -> 1
    , strokeDashArray = f_
    , fill = \_ -> Color.white
    , title = f_
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


tolerance : Float
tolerance =
    1.0


{-| Standard Edge Drawer. It can be configured using Rander.StandardDrawers.Attributes
-}
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
                    getAdjustedSrcAndTarget edgeAtrib 1.5 1.5
            in
            case config.linkStyle of
                Spline ->
                    Curve.catmullRom config.alpha pts

                Polyline ->
                    Curve.linear pts

        parameterizedCurve =
            SP.arcLengthParameterized tolerance curve

        edgeId =
            "edge-" ++ String.fromInt edge.from ++ "-" ++ String.fromInt edge.to

        edgePathId =
            edgeId ++ "-path"

        gAtrib =
            case config.onClick of
                Nothing ->
                    [ TA.id edgeId
                    , TA.class [ "edge" ]
                    , TA.style <| config.style edge
                    ]

                Just f ->
                    [ TA.id edgeId
                    , TA.class [ "edge" ]
                    , TA.style <| config.style edge
                    , TE.onClick (f edge)
                    , TA.cursor TT.CursorPointer
                    ]
    in
    g
        gAtrib
        [ arrowHeadDef config.arrowHead (config.strokeColor edge)
        , TS.title [] [ TC.text <| config.title edge ]
        , TS.path
            [ TA.id edgePathId
            , TA.d <| SP.toString curve
            , TA.stroke <| TT.Paint <| config.strokeColor edge
            , TA.strokeWidth <| TT.Px <| config.strokeWidth edge
            , TA.strokeDasharray <| config.strokeDashArray edge
            , TA.fill TT.PaintNone
            , TA.markerEnd (arrowHeadId config.arrowHead)
            ]
            []
        , edgeLabelDrawer (config.label edge) config.fontSize config.orientLabelAlongEdge edgePathId parameterizedCurve
        ]


{-| Standard Node Drawer. It can be configured using Rander.StandardDrawers.Attributes
-}
svgDrawNode : List (Attribute (NodeDrawerConfig n msg)) -> NodeDrawer n msg
svgDrawNode edits nodeAtrib =
    let
        node =
            nodeAtrib.node

        ( posX, posY ) =
            nodeAtrib.coord

        config =
            List.foldl (\f a -> f a) defNodeDrawerConfig edits

        lbl =
            config.label node

        nodeId =
            "node-" ++ String.fromInt node.id

        gAtrib =
            case config.onClick of
                Nothing ->
                    [ TA.id nodeId
                    , TA.class [ "node" ]
                    , TA.style <| config.style node
                    ]

                Just f ->
                    [ TA.id nodeId
                    , TA.class [ "node" ]
                    , TA.style <| config.style node
                    , TE.onClick (f node)
                    , TA.cursor TT.CursorPointer
                    ]

        shapeAtrib =
            { shape = config.shape
            , strokeColor = config.strokeColor
            , strokeWidth = config.strokeWidth
            , strokeDashArray = config.strokeDashArray
            , fill = config.fill
            }
    in
    g
        gAtrib
        [ TS.title [] [ TC.text <| config.title node ]
        , nodeShapeDrawer shapeAtrib nodeAtrib
        , centeredText lbl config.fontSize ( posX, posY )
        , xLabelsDrawer config.xLabels nodeAtrib
        ]


{-| Standard xLabel Drawer for NodeDrawer's xLabel attribute.
It can be configured using Rander.StandardDrawers.Attributes
-}
svgDrawXLabel : List (Attribute (XLabelDrawerConfig n)) -> NodeAttributes n -> Svg msg
svgDrawXLabel edits nodeAtrib =
    let
        config =
            List.foldl (\f a -> f a) defXLabelDrawerConfig edits

        ( posX, posY ) =
            nodeAtrib.coord

        ( xPosX, xPosY ) =
            config.pos nodeAtrib.node nodeAtrib.width nodeAtrib.height

        ( xlPosX, xlPosY ) =
            ( posX + xPosX, posY + xPosY )

        nodeAtribX =
            { nodeAtrib
                | width = nodeAtrib.width / 2
                , height = nodeAtrib.height / 2
                , coord = ( xlPosX, xlPosY )
            }

        shapeAtrib =
            { shape = config.shape
            , strokeColor = config.strokeColor
            , strokeWidth = config.strokeWidth
            , strokeDashArray = config.strokeDashArray
            , fill = config.fill
            }
    in
    TS.g
        [ TA.class [ "xlabel" ] ]
        [ TS.title [] [ TC.text <| config.title nodeAtribX.node ]
        , nodeShapeDrawer shapeAtrib nodeAtribX
        , centeredText (config.label nodeAtribX.node) config.fontSize ( xlPosX, xlPosY )
        ]


centeredText : String -> Float -> ( Float, Float ) -> Svg msg
centeredText str fontSize ( posX, posY ) =
    TS.text_
        [ TA.textAnchor TT.AnchorMiddle
        , TA.dominantBaseline TT.DominantBaselineCentral
        , TA.transform [ TT.Translate posX posY ]
        , TA.fontSize <| TT.Px fontSize
        ]
        [ TC.text str
        ]



{- Gives coordinates of the edge adjusted with the srcMargin,TgtMargin and arrowhead lengths -}


getAdjustedSrcAndTarget : EdgeAttributes e -> Float -> Float -> List ( Float, Float )
getAdjustedSrcAndTarget edgeAtrib srcMargin tgtMargin =
    let
        pts =
            List.concat [ [ edgeAtrib.source ], edgeAtrib.controlPts, [ edgeAtrib.target ] ]

        ( sw, sh ) =
            edgeAtrib.sourceDimensions

        ( tw, th ) =
            edgeAtrib.targetDimensions

        src_to_next =
            List.take 2 pts

        target_from_previous =
            List.drop (List.length pts - 2) pts

        srcSeg =
            Curve.linear src_to_next |> SP.arcLengthParameterized tolerance

        tgtSeg =
            Curve.linear target_from_previous |> SP.arcLengthParameterized tolerance

        srcDim =
            (((sw ^ 2 + sh ^ 2) |> sqrt) / 2) + srcMargin

        tgtDim =
            (((tw ^ 2 + th ^ 2) |> sqrt) / 2) + tgtMargin

        final_src =
            SP.pointAlong srcSeg srcDim

        final_tgt =
            SP.pointAlong tgtSeg (SP.arcLength tgtSeg - tgtDim)
    in
    if edgeAtrib.edge.from == edgeAtrib.edge.to then
        pts

    else
        case ( final_src, final_tgt ) of
            ( Just s, Just t ) ->
                List.concat [ [ s ], edgeAtrib.controlPts, [ t ] ]

            ( Just s, Nothing ) ->
                List.concat [ [ s ], edgeAtrib.controlPts, [ edgeAtrib.target ] ]

            ( Nothing, Just t ) ->
                List.concat [ [ edgeAtrib.source ], edgeAtrib.controlPts, [ t ] ]

            ( Nothing, Nothing ) ->
                pts


arrowHeadDef : ArrowHeadShape -> Color.Color -> Svg msg
arrowHeadDef ahs stroke =
    case ahs of
        Triangle ->
            TS.defs [] [ triangleHeadElement stroke ]

        Vee ->
            TS.defs [] [ veeHeadElement stroke ]

        None ->
            TS.defs [] []


edgeLabelDrawer : String -> Float -> Bool -> String -> SP.ArcLengthParameterized c -> Svg msg
edgeLabelDrawer lbl fontSize orientLabelAlongEdge edgePathId curve =
    if orientLabelAlongEdge then
        TS.text_
            [ TA.textAnchor TT.AnchorMiddle
            , TA.dominantBaseline TT.DominantBaselineCentral
            , TA.fontSize <| TT.Px fontSize
            ]
            [ TS.textPath [ TA.href ("#" ++ edgePathId), TA.startOffset "50%" ] [ TC.text lbl ] ]

    else
        let
            ( midX, midY ) =
                (case SP.pointAlong curve (SP.arcLength curve / 2) of
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
        in
        centeredText lbl fontSize ( midX, midY )


nodeShapeDrawer : ShapeAttributes n -> NodeAttributes n -> Svg msg
nodeShapeDrawer config nodeAtrib =
    let
        ( posX, posY ) =
            nodeAtrib.coord

        width =
            nodeAtrib.width

        height =
            nodeAtrib.height

        d =
            max width height
    in
    case config.shape nodeAtrib.node of
        Circle ->
            TS.circle
                [ TA.r <| TT.Px (d / 2)
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.cx <| TT.Px posX
                , TA.cy <| TT.Px posY
                ]
                []

        Ellipse ->
            TS.ellipse
                [ TA.rx <| TT.Px (width / 2)
                , TA.ry <| TT.Px (height / 2)
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.cx <| TT.Px posX
                , TA.cy <| TT.Px posY
                ]
                []

        Box ->
            TS.rect
                [ TA.width <| TT.Px width
                , TA.height <| TT.Px height
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.x <| TT.Px (posX - width / 2)
                , TA.y <| TT.Px (posY - height / 2)
                ]
                []

        RoundedBox r ->
            TS.rect
                [ TA.width <| TT.Px width
                , TA.height <| TT.Px height
                , TA.rx <| TT.Px r
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.x <| TT.Px (posX - width / 2)
                , TA.y <| TT.Px (posY - height / 2)
                ]
                []

        NoShape ->
            TS.g
                []
                []


xLabelsDrawer : List (NodeAttributes n -> Svg msg) -> NodeAttributes n -> Svg msg
xLabelsDrawer xLabelDrawers nodeAtrib =
    TS.g
        [ TA.class [ "xlabels" ] ]
        (List.map (\f -> f nodeAtrib) xLabelDrawers)



{- Different svg elements for different heads on edges -}


triangleHeadElement : Color.Color -> Svg msg
triangleHeadElement stroke =
    TS.marker
        [ TA.id "triangle-head"
        , TA.viewBox 0 0 9 6
        , TA.markerWidth <| TT.Px 4.5
        , TA.markerHeight <| TT.Px 3
        , TA.refX "6"
        , TA.refY "3"
        , TA.orient "auto"
        , TA.markerUnits TT.MarkerCoordinateSystemStrokeWidth
        ]
        [ TS.polygon
            [ TA.points [ ( 0, 0 ), ( 0, 6 ), ( 9, 3 ) ]
            , TA.stroke <| TT.Paint stroke
            , TA.fill <| TT.Paint stroke
            ]
            []
        ]


veeHeadElement : Color.Color -> Svg msg
veeHeadElement stroke =
    TS.marker
        [ TA.id "vee-head"
        , TA.viewBox 0 0 9 6
        , TA.markerWidth <| TT.Px 4.5
        , TA.markerHeight <| TT.Px 3
        , TA.refX "6"
        , TA.refY "3"
        , TA.orient "auto"
        , TA.markerUnits TT.MarkerCoordinateSystemStrokeWidth
        ]
        [ TS.polygon
            [ TA.points [ ( 0, 0 ), ( 4.5, 3 ), ( 0, 6 ), ( 9, 3 ) ]
            , TA.stroke <| TT.Paint stroke
            , TA.fill <| TT.Paint stroke
            ]
            []
        ]

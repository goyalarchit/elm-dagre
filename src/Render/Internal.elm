module Render.Internal exposing (..)

{-| This module is a functional implementation of the method I found here
<https://francoisromain.medium.com/smooth-a-svg-path-with-cubic-bezier-curves-e37b49d46c74>

@deprecated I found a better module named "folkertdev/one-true-path-experiment" to draw splines.

-}

import TypedSvg.Attributes as TA
import TypedSvg.Core as TC


type LinkStyle
    = Spline
    | PolyLine


type alias Coord =
    ( Float, Float )


fromCoord : Coord -> String
fromCoord ( x, y ) =
    " "
        ++ String.fromFloat x
        ++ ","
        ++ String.fromFloat y
        ++ " "


spline : Float -> Coord -> Coord -> List Coord -> TC.Attribute msg
spline smooth src tgt controlPts =
    let
        anchorPoints =
            getConsecutiveAnchorPoints ( src, controlPts, tgt )

        startAPs =
            List.take (List.length anchorPoints - 1) anchorPoints

        endAPs =
            List.drop 1 anchorPoints

        bezierSegments =
            List.map2 (getCubicBezier smooth) startAPs endAPs
    in
    TA.d
        ("M"
            ++ fromCoord src
            ++ String.join " " bezierSegments
        )


getConsecutiveAnchorPoints : ( Coord, List Coord, Coord ) -> List ( Coord, Coord, Coord )
getConsecutiveAnchorPoints ( src, controlPts, tgt ) =
    let
        points =
            List.concat [ [ src ], controlPts, [ tgt ] ]

        pointsCnt =
            List.length points

        prevAPs =
            src :: List.take (pointsCnt - 1) points

        nextAPs =
            List.append (List.drop 1 points) [ tgt ]
    in
    List.map3 (\c p n -> ( c, p, n )) points prevAPs nextAPs


getCubicBezier : Float -> ( Coord, Coord, Coord ) -> ( Coord, Coord, Coord ) -> String
getCubicBezier smooth startAP endAP =
    let
        ( endPt, _, _ ) =
            endAP

        cps =
            getControlPoint smooth startAP False

        cpe =
            getControlPoint smooth endAP True
    in
    "C" ++ fromCoord cps ++ fromCoord cpe ++ fromCoord endPt


getControlPoint : Float -> ( Coord, Coord, Coord ) -> Bool -> Coord
getControlPoint smooth ( current, prev, next ) rev =
    let
        ( curX, curY ) =
            current

        smoothing =
            smooth

        ( len, angle ) =
            line prev next

        finalAngle =
            if rev then
                angle + pi

            else
                angle

        finLen =
            len * smoothing

        x =
            curX + cos finalAngle * finLen

        y =
            curY + sin finalAngle * finLen
    in
    ( x, y )


line : Coord -> Coord -> ( Float, Float )
line ( aX, aY ) ( bX, bY ) =
    let
        lenX =
            bX - aX

        lenY =
            bY - aY
    in
    ( sqrt ((lenX ^ 2) + (lenY ^ 2))
    , atan2 lenY lenX
    )

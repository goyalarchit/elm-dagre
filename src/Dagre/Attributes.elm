module Dagre.Attributes exposing (..)

import Dict exposing (Dict)
import Graph as G


type alias Config =
    { rankDir : RankDir
    , widthDict : Dict G.NodeId Float
    , heightDict : Dict G.NodeId Float
    , width : Float
    , height : Float
    , nodeSep : Float
    , edgeSep : Float
    , rankSep : Float
    , marginX : Float
    , marginY : Float
    }


type RankDir
    = TB
    | BT
    | LR
    | RL


type alias Attribute =
    Config -> Config


rankDir : RankDir -> Attribute
rankDir rDir =
    \a ->
        { a | rankDir = rDir }



{-
   Note :
   1. The following attributes can not have values < 0
   2. If value "v" < 0 then absolute value of "v" is used
-}


widthDict : Dict G.NodeId Float -> Attribute
widthDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | widthDict = absDict }


heightDict : Dict G.NodeId Float -> Attribute
heightDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | heightDict = absDict }


width : Float -> Attribute
width w =
    \a ->
        { a | width = abs w }


height : Float -> Attribute
height h =
    \a ->
        { a | height = abs h }


nodeSep : Float -> Attribute
nodeSep nSep =
    \a ->
        { a | nodeSep = abs nSep }


edgeSep : Float -> Attribute
edgeSep eSep =
    \a ->
        { a | edgeSep = abs eSep }


rankSep : Float -> Attribute
rankSep rSep =
    \a ->
        { a | rankSep = abs rSep }


marginX : Float -> Attribute
marginX mX =
    \a ->
        { a | marginX = abs mX }


marginY : Float -> Attribute
marginY mY =
    \a ->
        { a | marginY = abs mY }

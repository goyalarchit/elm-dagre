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


widthDict : Dict G.NodeId Float -> Attribute
widthDict dict =
    \a ->
        { a | widthDict = dict }


heightDict : Dict G.NodeId Float -> Attribute
heightDict dict =
    \a ->
        { a | heightDict = dict }


width : Float -> Attribute
width w =
    \a ->
        { a | width = w }


height : Float -> Attribute
height h =
    \a ->
        { a | height = h }


nodeSep : Float -> Attribute
nodeSep nSep =
    \a ->
        { a | nodeSep = nSep }


edgeSep : Float -> Attribute
edgeSep eSep =
    \a ->
        { a | edgeSep = eSep }


rankSep : Float -> Attribute
rankSep rDir =
    \a ->
        { a | rankSep = rDir }


marginX : Float -> Attribute
marginX mX =
    \a ->
        { a | marginX = mX }


marginY : Float -> Attribute
marginY mY =
    \a ->
        { a | marginY = mY }

module Dagre.Attributes exposing (..)

import Dict exposing (Dict)
import Graph as G


type alias Attributes =
    { rankDir : RankDir
    , widthDict : Dimension
    , heightDict : Dimension
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


type alias Dimension =
    Dict G.NodeId Float


type alias Attribute =
    Attributes -> Attributes


rankDir : RankDir -> Attribute
rankDir rDir =
    \a ->
        { a | rankDir = rDir }


widthDict : Dimension -> Attribute
widthDict dict =
    \a ->
        { a | widthDict = dict }


heightDict : Dimension -> Attribute
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

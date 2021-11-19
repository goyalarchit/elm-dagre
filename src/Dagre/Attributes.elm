module Dagre.Attributes exposing (..)

{-| Dagre Configuration Attributes


# Types

@docs RankDir, Config, Attribute


# Attributes

These function set the respective attributes for the algorithm

**Note** : All attributes except rankDir can not have values < 0. If value "v" < 0
then absolute value of "v" is used

@docs rankDir, widthDict, heightDict, width, height, nodeSep, edgeSep, rankSep, marginX, marginY

-}

import Dict exposing (Dict)
import Graph as G


{-| This type represents the config for the dagre layout algorithm

**Note** : For complete info on default values and description of each field, please
see Dagre module.

-}
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


{-| This type represents the rank directions.
-}
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

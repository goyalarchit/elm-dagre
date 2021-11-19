module Dagre.Attributes exposing
    ( RankDir(..), Config, Attribute
    , rankDir, widthDict, heightDict, width, height, nodeSep, edgeSep, rankSep, marginX, marginY
    )

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
T,L,B,R represent Top, Left, Bottom, Right respectively.
TB represent Top to Bottom. Similar notations for others.
-}
type RankDir
    = TB
    | BT
    | LR
    | RL


{-| Attribute type for Dagre
-}
type alias Attribute =
    Config -> Config


{-| Sets the rank direction of layout
-}
rankDir : RankDir -> Attribute
rankDir rDir =
    \a ->
        { a | rankDir = rDir }


{-| Sets the widthDict for nodes in layout
-}
widthDict : Dict G.NodeId Float -> Attribute
widthDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | widthDict = absDict }


{-| Sets the heightDict for nodes in layout
-}
heightDict : Dict G.NodeId Float -> Attribute
heightDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | heightDict = absDict }


{-| Sets the default width for nodes in layout
-}
width : Float -> Attribute
width w =
    \a ->
        { a | width = abs w }


{-| Sets the default height for nodes in layout
-}
height : Float -> Attribute
height h =
    \a ->
        { a | height = abs h }


{-| Sets the node separation for layout
-}
nodeSep : Float -> Attribute
nodeSep nSep =
    \a ->
        { a | nodeSep = abs nSep }


{-| Sets the edge separation for layout
-}
edgeSep : Float -> Attribute
edgeSep eSep =
    \a ->
        { a | edgeSep = abs eSep }


{-| Sets the rank separation for layout
-}
rankSep : Float -> Attribute
rankSep rSep =
    \a ->
        { a | rankSep = abs rSep }


{-| Sets the x margin for graph in layout
-}
marginX : Float -> Attribute
marginX mX =
    \a ->
        { a | marginX = abs mX }


{-| Sets the y margin for graph in layout
-}
marginY : Float -> Attribute
marginY mY =
    \a ->
        { a | marginY = abs mY }

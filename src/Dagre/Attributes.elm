module Dagre.Attributes exposing
    ( RankDir(..), Config, Attribute
    , rankDir, widthDict, heightDict, width, height, nodeSep, edgeSep, rankSep, marginX, marginY
    )

{-| Dagre Configuration Attributes


# Types

@docs RankDir, Config, Attribute


# Attributes

These function set the respective attributes for the algorithm

**Note** :

1.  All numeric attributes can not have values < 0. If a value `v` < 0
    then absolute value of `v` is used.
2.  All numeric values are defined in pixels.

@docs rankDir, widthDict, heightDict, width, height, nodeSep, edgeSep, rankSep, marginX, marginY

-}

import Dict exposing (Dict)
import Graph as G


{-| This type represents the config for the dagre layout algorithm

**Note** : For complete info on default values and description of each field, please
see associated attributes.

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
TB represent Top to Bottom direction. Similar notation is
used for other directions.
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


{-| The rankDir defines the direction for rank nodes.

The possible values can be TB, BT, LR, or RL.

The default value is TB

-}
rankDir : RankDir -> Attribute
rankDir rDir =
    \a ->
        { a | rankDir = rDir }


{-| The widthDict associates nodes with a width that will be used during the layout.

    -- For example you want nodes 1,2,3 with widths 50,60,70 respectively then
    -- you can pass the following
    let
        widths =
            Dict.fromList [ ( 1, 50 ), ( 2, 60 ), ( 3, 70 ) ]
    in
    runLayout [ widthDict widths ] simplegraph

The dafualt value is Dict.empty

-}
widthDict : Dict G.NodeId Float -> Attribute
widthDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | widthDict = absDict }


{-| The heightDict associates nodes with a height that will be used during the layout.

    -- For example you want nodes 1,2,3 with heights 30,50,40 respectively then
    -- you can pass the following
    let
        heights =
            Dict.fromList [ ( 1, 30 ), ( 2, 50 ), ( 3, 40 ) ]
    in
    runLayout [ heightDict heights ] simplegraph

The dafualt value is Dict.empty

-}
heightDict : Dict G.NodeId Float -> Attribute
heightDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | heightDict = absDict }


{-| Defines the default width that will be used during the layout.
This value will be used when no value is available in widthDict for some node.

The default value of width is 32 pixels.

-}
width : Float -> Attribute
width w =
    \a ->
        { a | width = abs w }


{-| Defines the default height that will be used during the layout.
This value will be used when no value is available in heightDict for some node.

The default value of height is 32 pixels.

-}
height : Float -> Attribute
height h =
    \a ->
        { a | height = abs h }


{-| Defines the number of pixels that separate nodes in same rank in the layout.

The default value is 50 pixel.

-}
nodeSep : Float -> Attribute
nodeSep nSep =
    \a ->
        { a | nodeSep = abs nSep }


{-| Defines the number of pixels that separate edges horizontally in the layout.

The default value is 10 pixel.

-}
edgeSep : Float -> Attribute
edgeSep eSep =
    \a ->
        { a | edgeSep = abs eSep }


{-| Defines number of pixels between each rank in the layout.

The default value is 75 pixel.

-}
rankSep : Float -> Attribute
rankSep rSep =
    \a ->
        { a | rankSep = abs rSep }


{-| Defines the number of pixels to use as a margin around the left and right
of the graph.

The default value is 20 pixels.

-}
marginX : Float -> Attribute
marginX mX =
    \a ->
        { a | marginX = abs mX }


{-| Defines the number of pixels to use as a margin around the top and bottom
of the graph.

The default value is 20 pixel.

-}
marginY : Float -> Attribute
marginY mY =
    \a ->
        { a | marginY = abs mY }

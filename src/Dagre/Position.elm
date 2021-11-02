module Dagre.Position exposing (..)

import Dagre.Attributes exposing (RankDir(..))
import Dagre.Position.BK as BK exposing (NodePointDict)
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G



{-
   Naming Convention
   If we get a value from dictionary "dict" for key "v",
   and store it in a variable, then the name of variable should be
   dict_v == dict[v]
   the variable holding the maybe value is named
   "dict_v_"

   If we update the key "v" in a dictionary "dict",
   then the new updated dictionary is stored in
   "dictV"

-}
{-
   map all the nodes in adjacent layer to their order, and the edges to orders too


   after marking one layer's conflicting edges remap them to vertices

-}


positionY : List DU.Layer -> Float -> Float -> NodePointDict
positionY rankList rankSep maxHeight =
    let
        ys =
            Dict.empty

        ( _, ys_assigned ) =
            List.foldl (assignAbsoluteY rankSep maxHeight) ( 0, ys ) rankList
    in
    ys_assigned



{-
   MaxHeight is the maximum height of Node in that layer
-}


assignAbsoluteY : Float -> Float -> DU.Layer -> ( Float, NodePointDict ) -> ( Float, NodePointDict )
assignAbsoluteY rankSep maxHeight l ( currentY, ys ) =
    let
        ys_updated =
            List.foldl (\n ys_ -> Dict.insert n (currentY + maxHeight / 2) ys_) ys l

        newY =
            currentY + maxHeight + rankSep
    in
    ( newY, ys_updated )



{-
   Helper to combine X and Y coordinates
-}


combinePoints : NodePointDict -> NodePointDict -> Dict G.NodeId DU.Coordinates
combinePoints xs ys =
    let
        onlyX =
            \n x finalDict -> Dict.insert n ( x, 0 ) finalDict

        bothXY =
            \n x y finalDict -> Dict.insert n ( x, y ) finalDict

        onlyY =
            \n y finalDict -> Dict.insert n ( 0, y ) finalDict
    in
    Dict.merge onlyX bothXY onlyY xs ys Dict.empty


position : G.Graph n e -> ( List DU.Layer, List DU.Edge ) -> Dict G.NodeId DU.Coordinates
position g ( rankList, edges ) =
    let
        -- TODO : Add the step to swap heights and widths ditcs if the rankDir = LR or RL
        ys =
            positionY rankList 50 0

        xs =
            BK.positionX g ( rankList, edges )

        init_coords =
            combinePoints xs ys

        final_coords =
            applyRankDir LR init_coords

        -- TODO : Add translate function to translate the coordinates and add graph margins
    in
    final_coords


applyRankDir : RankDir -> Dict G.NodeId DU.Coordinates -> Dict G.NodeId DU.Coordinates
applyRankDir rankDir init_coords =
    let
        coords_ =
            if rankDir == BT || rankDir == RL then
                Dict.map (\_ ( x, y ) -> ( x, -y )) init_coords

            else
                init_coords
    in
    if rankDir == LR || rankDir == RL then
        Dict.map (\_ ( x, y ) -> ( y, x )) coords_

    else
        coords_

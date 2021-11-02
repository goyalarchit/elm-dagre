module Dagre.Position exposing (position)

import Dagre.Attributes as DA exposing (RankDir(..))
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


positionY : DA.Config -> List DU.Layer -> NodePointDict
positionY config rankList =
    let
        ys =
            Dict.empty

        ( _, ys_assigned ) =
            List.foldl (assignAbsoluteY config) ( 0, ys ) rankList
    in
    ys_assigned



{-
   MaxHeight is the maximum height of Node in that layer
-}


assignAbsoluteY : DA.Config -> DU.Layer -> ( Float, NodePointDict ) -> ( Float, NodePointDict )
assignAbsoluteY config l ( currentY, ys ) =
    let
        getHeight =
            \n ->
                Dict.get n config.heightDict
                    |> Maybe.withDefault config.height

        maxHeight =
            List.map getHeight l
                |> List.maximum
                |> Maybe.withDefault config.height

        ys_updated =
            List.foldl (\n ys_ -> Dict.insert n (currentY + maxHeight / 2) ys_) ys l

        newY =
            currentY + maxHeight + config.rankSep
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


position : DA.Config -> G.Graph n e -> ( List DU.Layer, List DU.Edge ) -> Dict G.NodeId DU.Coordinates
position config g ( rankList, edges ) =
    let
        adjustedConfig =
            if config.rankDir == LR || config.rankDir == RL then
                { config
                    | widthDict = config.heightDict
                    , heightDict = config.widthDict
                    , width = config.height
                    , height = config.width
                }

            else
                config

        ys =
            positionY adjustedConfig rankList

        xs =
            BK.positionX adjustedConfig g ( rankList, edges )

        init_coords =
            combinePoints xs ys

        final_coords =
            applyRankDir config.rankDir init_coords

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

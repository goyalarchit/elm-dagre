module Dagre.Position exposing (position)

import Dagre.Attributes as DA exposing (RankDir(..))
import Dagre.Position.BK as BK exposing (NodePointDict)
import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G
import List.Extra as LE


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
            height config

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


height : DA.Config -> (G.NodeId -> Float)
height config =
    \n ->
        Dict.get n config.heightDict
            |> Maybe.withDefault config.height



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


position : DA.Config -> G.Graph n e -> ( List DU.Layer, List DU.Edge ) -> ( Dict G.NodeId DU.Coordinates, ( Float, Float ) )
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
            applyRankDir adjustedConfig.rankDir init_coords
    in
    translate config final_coords


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


translate : DA.Config -> Dict G.NodeId DU.Coordinates -> ( Dict G.NodeId DU.Coordinates, ( Float, Float ) )
translate config coords =
    let
        getWidth =
            BK.width config

        getHeight =
            height config

        coordsWithMinXY =
            Dict.map
                (\n ( x, y ) -> ( x - getWidth n / 2, y - getHeight n / 2 ))
                coords
                |> Dict.values

        coordsWithMaxXY =
            Dict.map
                (\n ( x, y ) -> ( x + getWidth n / 2, y + getHeight n / 2 ))
                coords
                |> Dict.values

        minX =
            (LE.minimumBy Tuple.first coordsWithMinXY
                |> Maybe.withDefault ( 0, 0 )
                |> Tuple.first
            )
                - config.marginX

        minY =
            (LE.minimumBy Tuple.second coordsWithMinXY
                |> Maybe.withDefault ( 0, 0 )
                |> Tuple.second
            )
                - config.marginY

        maxX =
            (LE.maximumBy Tuple.first coordsWithMaxXY
                |> Maybe.withDefault ( 500, 500 )
                |> Tuple.first
            )
                - minX
                + config.marginX

        maxY =
            (LE.maximumBy Tuple.second coordsWithMaxXY
                |> Maybe.withDefault ( 500, 500 )
                |> Tuple.second
            )
                - minY
                + config.marginY
    in
    ( Dict.map (\_ ( x, y ) -> ( x - minX, y - minY )) coords, ( maxX, maxY ) )

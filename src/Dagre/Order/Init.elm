module Dagre.Order.Init exposing (initOrder)

import Dagre.Layer as DL
import List



{-
   Gives initial order for all layers by sorting them based on Node Ids
-}


initOrder : List DL.Layer -> List DL.Layer
initOrder layering =
    List.map
        (\layer ->
            DL.sortBy identity layer
        )
        layering

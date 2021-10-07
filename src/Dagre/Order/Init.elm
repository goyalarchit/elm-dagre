module Dagre.Order.Init exposing (initOrder)

import Dagre.Utils as DU
import List



{-
   Gives initial order for all layers by sorting them based on Node Ids
-}


initOrder : List DU.Layer -> List DU.Layer
initOrder layering =
    List.map List.sort layering

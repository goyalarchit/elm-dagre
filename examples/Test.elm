module Test exposing (..)

-- import Html.Attributes exposing()

import Graph as G
import Render as R



{-
   The following tree verifies that Dagre.Order.Transpose works
   But also gives idea that graphviz can draw it with no crossing,
   while elm-dagre draws it with 1 crossing edge.
-}


tree : G.Graph Int ()
tree =
    G.fromNodeLabelsAndEdgePairs
        [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
        [ ( 0, 1 )
        , ( 0, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 2, 5 )
        , ( 2, 6 )
        , ( 3, 7 )
        , ( 3, 8 )
        , ( 0, 7 )
        , ( 0, 5 )
        , ( 0, 6 )
        , ( 4, 0 )

        -- , ( 6, 0 )
        -- , ( 7, 0 )
        -- , ( 4, 8 )
        -- , ( 6, 8 )
        -- , ( 7, 8 )
        ]


main =
    R.draw R.svgDrawNode R.svgDrawEdge tree
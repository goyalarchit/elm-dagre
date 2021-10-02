module Test exposing (..)

-- import Html.Attributes exposing()

import Dagre as D
import Graph as G
import Render as R


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

        -- , ( 4, 8 )
        -- , ( 6, 8 )
        -- , ( 7, 8 )
        ]


main =
    R.draw R.svgDrawNode R.svgDrawEdge D.acg

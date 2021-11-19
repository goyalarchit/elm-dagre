module Simple exposing (..)

import Graph as G
import Html
import Render as R


simpleGraph : G.Graph Int ()
simpleGraph =
    G.fromNodeLabelsAndEdgePairs
        [ 0, 1, 2, 3, 4, 5, 6, 7 ]
        [ ( 3, 4 )
        , ( 2, 0 )
        , ( 6, 1 )
        , ( 3, 6 )
        , ( 1, 3 )
        , ( 3, 0 )
        , ( 1, 2 )
        , ( 7, 2 )
        , ( 1, 1 )
        , ( 4, 1 )
        , ( 0, 7 )
        , ( 4, 5 )
        , ( 3, 2 )
        , ( 6, 7 )
        , ( 1, 7 )
        , ( 3, 0 )
        , ( 6, 1 )
        , ( 4, 3 )
        , ( 0, 4 )
        , ( 6, 0 )
        ]


main : Html.Html msg
main =
    R.draw
        []
        [ R.style "height: 100vh;"
        ]
        simpleGraph

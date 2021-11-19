# elm-dagre

Sugiyama Style graph drawing in pure elm. It is an elm implementation of famous js library [dagrejs](https://github.com/dagrejs/dagre).

The package exposes two modules

1. Dagre : The main graph layout algorithm
2. Render : Uses Dagre to draw graphs

In most use cases you will need to use Render. The Dagre is exposed in case you want to embed this in some other rendering library.

See an [example in action on Ellie]({{YOUR LINK HERE}}).

See more end-to-end example code in the `examples/` folder.

## Design Goals

1. Implement pure elm based Graph layout algorithm with all configurations options that are offered by the popular javascript equivalents.
2. A minimalistic rendering/drawing api for drawing graphs. The api supports
   1. Adding Onclick Events
   2. Customising the aesthetics of nodes and edges.

## Overview

```elm
import Dagre.Attributes as DA
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
        [ DA.width 32
        , DA.height 32
        , DA.marginX 3
        , DA.marginY 3
        , DA.rankSep 75
        ]
        [ R.style "height: 100vh;"
        ]
        simpleGraph
```

- Link to live [Ellie](https://ellie-app.com/) Demo

## Usage

## Learning Resources

Please visit the [elm-dagre's wiki](https://github.com/goyalarchit/elm-dagre/wiki) for learning more about the library and contributing to it.

If you find a bug please report it using the github issues.

## Future Work

1. Write Test cases using elm-test.
2. Use elm-explorations/benchmark to benchmark the performance of both Dagre and Draw modules.
3. Add layout support to Dagre for the following
   1. Self loops
   2. Parallel Edges

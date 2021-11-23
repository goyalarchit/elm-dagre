# elm-dagre

Sugiyama Style graph drawing in pure elm. It is an elm implementation of popular js library [dagrejs](https://github.com/dagrejs/dagre). The package can be use to draw all kinds of graphs including trees efficiently.

The package exposes two modules

1. Dagre : The main graph layout algorithm
2. Render : Uses Dagre to draw graphs

In most use cases you will need to use Render. The Dagre is exposed in case you want to embed this in some other rendering library.

See an [example in action on Ellie](https://ellie-app.com/fTXMq6XgDbpa1).

See more end-to-end example code in the `examples/` folder.

## Design Goals

1. Implement pure elm based graph layout algorithm with all configurations options that are offered by the popular javascript equivalents.
2. The dagre tries to achieve the following aesthetic principles as described in [A Technique for Drawing Directed Graphs](http://www.graphviz.org/documentation/TSE93.pdf)
   1. Expose hierarchical structure in the graph. In particular, **aim edges in the same general direction if possible**. This aids finding directed paths and highlights source and sink nodes.
   2. Avoid visual anomalies that do not convey information about the underlying graph. For example, **avoid edge crossings and sharp bends**.
   3. Keep edges short. This makes it easier to find related nodes and contributes to A2.
   4. Favor symmetry and balance. This aesthetic has a secondary role in a few places in our algorithm.
3. A minimalistic rendering/drawing api for drawing graphs. The api supports
   1. Adding Onclick Events
   2. Customising the aesthetics of nodes and edges etc.

## Overview

```elm
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
        [ ]
        [ R.style "height: 100vh;"
        ]
        simpleGraph
```

- Link to live [Ellie](https://ellie-app.com/fTQRxcj4sdPa1) Demo

## Learning Resources

Please visit the [elm-dagre's wiki](https://github.com/goyalarchit/elm-dagre/wiki) for learning more about the library and contributing to it.

If you find a bug please report it using the github issues.

## Future Work

- [ ] Write Test cases using elm-test.
- [ ] Use elm-explorations/benchmark to benchmark the performance of both Dagre and Render modules.
- [ ] Add layout support to Dagre for the following
  - [ ] Self loops
  - [ ] Parallel Edges

## Dagrejs Attribution

This project is inspired from [dagrejs](https://github.com/dagrejs/dagre). Thanks contributors@dagrejs for your hard work. [(Original MIT License)](./LICENSE.dagrejs).

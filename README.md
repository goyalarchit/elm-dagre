# elm-dagre

Sugiyama Style graph drawing in pure elm. It is an elm implementation of popular js library [dagrejs](https://github.com/dagrejs/dagre).

The package exposes two modules

1. Dagre : The main graph layout algorithm
2. Render : Uses Dagre to draw graphs

In most use cases you will need to use Render. The Dagre is exposed in case you want to embed this in some other rendering library.

See an [example in action on Ellie]({{YOUR LINK HERE}}).

See more end-to-end example code in the `examples/` folder.

## Design Goals

1. Implement pure elm based graph layout algorithm with all configurations options that are offered by the popular javascript equivalents.
2. A minimalistic rendering/drawing api for drawing graphs. The api supports
   1. Adding Onclick Events
   2. Customising the aesthetics of nodes and edges.

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

- Link to live [Ellie](https://ellie-app.com/) Demo

## Dagre Configurations

Dagre supports the following configuration options.

**Note** : The table also list the default values.

| S. No | Field      | Default    | Description                                                                                                     |
| ----- | ---------- | ---------- | --------------------------------------------------------------------------------------------------------------- |
| 1.    | rankDir    | TB         | Direction for rank nodes. Can be `TB`, `BT`, `LR`, or `RL`, where T = top, B = bottom, L = left, and R = right. |
| 2.    | widthDict  | Dict.empty | Dict which maps node-ids to that node's width in pixels                                                         |
| 3.    | heightDict | Dict.empty | Dict which maps node-ids to that node's height in pixels                                                        |
| 4.    | height     | 32         | The default height of the node in pixels. Used when widthDict has no value for a node                           |
| 5.    | width      | 32         | The default width of the node in pixels. Used when heightDict has no value for a node                           |
| 6.    | nodeSep    | 50         | Number of pixels that separate nodes horizontally in the layout.                                                |
| 7.    | edgeSep    | 10         | Number of pixels that separate edges horizontally in the layout.                                                |
| 8.    | rankSep    | 75         | Number of pixels between each rank in the layout.                                                               |
| 9.    | marginX    | 0          | Number of pixels to use as a margin around the left and right of the graph.                                     |
| 10.   | marginY    | 0          | Number of pixels to use as a margin around the top and bottom of the graph.                                     |

You can configure above values by using functions in Dagre.Attributes.

## Learning Resources

Please visit the [elm-dagre's wiki](https://github.com/goyalarchit/elm-dagre/wiki) for learning more about the library and contributing to it.

If you find a bug please report it using the github issues.

## Future Work

- [ ] Write Test cases using elm-test.
- [ ] Use elm-explorations/benchmark to benchmark the performance of both Dagre and Render modules.
- [ ] Add layout support to Dagre for the following
  - [ ] Self loops
  - [ ] Parallel Edges

module GraphWithEvents2 exposing (..)

import Browser
import Dagre.Attributes as DA
import Graph as G
import Html
import Render as R
import Render.StandardDrawers as RSD
import Render.StandardDrawers.Attributes as RSDA
import Render.StandardDrawers.Types as RSDT


type alias Model =
    String


simpleGraph : G.Graph Int ()
simpleGraph =
    G.fromNodeLabelsAndEdgePairs
        [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
        [ ( 0, 1 )
        , ( 0, 2 )
        , ( 2, 0 )
        ]


type Msg
    = SelectEdge ( Int, Int )
    | SelectNode Int


init : Model
init =
    "No element selected!!, click on an edge/node to select it"


update : Msg -> Model -> Model
update msg _ =
    case msg of
        SelectNode v ->
            "You selected node " ++ String.fromInt v

        SelectEdge ( from, to ) ->
            "You selected edge from " ++ String.fromInt from ++ " to " ++ String.fromInt to


viewGraph : G.Graph n e -> Html.Html Msg
viewGraph g =
    R.draw
        [ DA.rankDir DA.LR
        ]
        -- []
        [ R.nodeDrawer
            (RSD.svgDrawNode
                [ RSDA.onClick (\n -> SelectNode n.id)
                ]
            )
        , R.edgeDrawer
            (RSD.svgDrawEdge
                [ RSDA.arrowHead RSDT.Vee
                , RSDA.onClick (\e -> SelectEdge ( e.from, e.to ))
                , RSDA.strokeWidth (\_ -> 4)
                ]
            )
        , R.style "height: 80vh;"
        ]
        g


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ viewGraph simpleGraph
        , Html.h1 [] [ Html.text model ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

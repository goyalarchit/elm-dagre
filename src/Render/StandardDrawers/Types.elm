module Render.StandardDrawers.Types exposing (..)

{-| This module provides the type used in Standard Drawers


# Standard Edge Drawer Specific Types

@docs ArrowHeadShape, LinkStyle


# Standard Node Drawer Specific Types

@docs Shape

-}


{-| This type represents the shape of arrow head to be used for the edge.
-}
type ArrowHeadShape
    = None
    | Triangle
    | Vee


{-| This type represents the style of line to be used for the edge.
-}
type LinkStyle
    = Polyline
    | Spline


{-| This type represents the shape of node.
-}
type Shape
    = Circle
    | Ellipse
    | Box
    | RoundedBox Float

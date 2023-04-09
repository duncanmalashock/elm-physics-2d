module Physics2d.Circle exposing
    ( Circle
    , new
    , radius
    )

{-|

@docs Circle
@docs new
@docs radius

-}

import Angle
import Length
import Physics2d.CoordinateSystem exposing (TopLeft)
import Point2d
import Quantity


type Circle
    = Circle Internals


type alias Internals =
    { radius : Length.Length
    }


new : { radius : Length.Length } -> Circle
new internals =
    Circle { radius = internals.radius }


radius :
    Circle
    -> Length.Length
radius (Circle internals) =
    internals.radius

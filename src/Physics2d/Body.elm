module Physics2d.Body exposing
    ( Body
    , fromPolygon
    , ShapeView(..)
    , view, View
    )

{-|

@docs Body
@docs fromPolygon
@docs ShapeView
@docs view, View

-}

import Angle
import Length
import LineSegment2d
import Physics2d.CoordinateSystem exposing (TopLeft)
import Physics2d.Polygon
import Point2d
import Vector2d


type Body
    = Body Internals


type Shape
    = PolygonShape Physics2d.Polygon.Polygon


type alias Internals =
    { shape : Shape
    , position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    }


fromPolygon :
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , polygon : Physics2d.Polygon.Polygon
    }
    -> Body
fromPolygon { position, rotation, polygon } =
    Body
        { position = position
        , rotation = rotation
        , shape = PolygonShape polygon
        }


type alias View =
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , shape : ShapeView
    }


type ShapeView
    = PolygonShapeView (List (Point2d.Point2d Length.Meters TopLeft))


view : Body -> View
view (Body internals) =
    { position = internals.position
    , rotation = internals.rotation
    , shape =
        case internals.shape of
            PolygonShape polygon ->
                PolygonShapeView
                    (Physics2d.Polygon.toPoints polygon
                        |> List.map
                            (Point2d.rotateAround
                                Point2d.origin
                                internals.rotation
                            )
                        |> List.map
                            (Point2d.translateBy
                                (Vector2d.from Point2d.origin internals.position)
                            )
                    )
    }

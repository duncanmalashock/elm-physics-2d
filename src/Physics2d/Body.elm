module Physics2d.Body exposing
    ( Body
    , fromPolygon, fromCircle
    , ShapeView(..)
    , view, View
    )

{-|

@docs Body
@docs fromPolygon, fromCircle
@docs ShapeView
@docs view, View

-}

import Angle
import Length
import LineSegment2d
import Physics2d.Circle
import Physics2d.CoordinateSystem exposing (TopLeft)
import Physics2d.Polygon
import Point2d
import Vector2d


type Body
    = Body Internals


type Shape
    = PolygonShape Physics2d.Polygon.Polygon
    | CircleShape Physics2d.Circle.Circle


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


fromCircle :
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , radius : Length.Length
    }
    -> Body
fromCircle { position, rotation, radius } =
    Body
        { position = position
        , rotation = rotation
        , shape =
            CircleShape
                (Physics2d.Circle.new
                    { radius = radius }
                )
        }


type alias View =
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , shape : ShapeView
    }


type ShapeView
    = PolygonShapeView (List (Point2d.Point2d Length.Meters TopLeft))
    | CircleShapeView { radius : Length.Length, position : Point2d.Point2d Length.Meters TopLeft }


view : Body -> View
view (Body internals) =
    { position = internals.position
    , rotation = internals.rotation
    , shape = toShapeView internals
    }


toShapeView : Internals -> ShapeView
toShapeView internals =
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

        CircleShape circle ->
            CircleShapeView
                { radius = Physics2d.Circle.radius circle
                , position = internals.position
                }

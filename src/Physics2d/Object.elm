module Physics2d.Object exposing
    ( Object
    , fromPolygon, fromCircle
    , velocity, setVelocity, addVelocity
    , setRotation
    , integrate
    , areColliding
    , ShapeView(..)
    , view, View
    )

{-|

@docs Object


# Constructors

@docs fromPolygon, fromCircle


# Applying changes

@docs velocity, setVelocity, addVelocity
@docs setRotation


# Integration

@docs integrate


# Collision

@docs areColliding


# View

@docs ShapeView
@docs view, View

-}

import Angle
import AngularSpeed
import Length
import LineSegment2d
import Physics2d.Circle
import Physics2d.CoordinateSystem exposing (TopLeft)
import Physics2d.Polygon
import Physics2d.Time
import Point2d
import Quantity
import Speed
import Vector2d


type Object
    = Object Internals


type Shape
    = PolygonShape Physics2d.Polygon.Polygon
    | CircleShape Physics2d.Circle.Circle


type alias Internals =
    { shape : Shape
    , position : Point2d.Point2d Length.Meters TopLeft
    , positionPrevious : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , rotationPrevious : Angle.Angle
    }


areColliding : Object -> Object -> Bool
areColliding (Object internals1) (Object internals2) =
    case ( internals1.shape, internals2.shape ) of
        ( CircleShape circle1, CircleShape circle2 ) ->
            let
                distanceBetweenCenters =
                    Point2d.distanceFrom internals1.position internals2.position

                radiusSum =
                    Quantity.sum
                        [ Physics2d.Circle.radius circle1
                        , Physics2d.Circle.radius circle2
                        ]
            in
            Quantity.lessThanOrEqualTo radiusSum distanceBetweenCenters

        _ ->
            False


fromPolygon :
    { position : Point2d.Point2d Length.Meters TopLeft
    , polygon : Physics2d.Polygon.Polygon
    }
    -> Object
fromPolygon { position, polygon } =
    initialInternals
        { position = position
        , rotation = Angle.turns 0
        }
        (PolygonShape polygon)
        |> Object


fromCircle :
    { position : Point2d.Point2d Length.Meters TopLeft
    , radius : Length.Length
    }
    -> Object
fromCircle { position, radius } =
    initialInternals
        { position = position
        , rotation = Angle.turns 0
        }
        (CircleShape
            (Physics2d.Circle.new
                { radius = radius }
            )
        )
        |> Object


initialInternals :
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    }
    -> shape
    ->
        { position : Point2d.Point2d Length.Meters TopLeft
        , positionPrevious : Point2d.Point2d Length.Meters TopLeft
        , rotation : Angle.Angle
        , rotationPrevious : Angle.Angle
        , shape : shape
        }
initialInternals config shape =
    let
        initialRotation =
            config.rotation
                |> Quantity.plus (Angle.turns 0.25)
    in
    { position = config.position
    , positionPrevious = config.position
    , rotation = initialRotation
    , rotationPrevious = initialRotation
    , shape = shape
    }


velocity : Object -> Vector2d.Vector2d Speed.MetersPerSecond TopLeft
velocity (Object internals) =
    Vector2d.from internals.positionPrevious internals.position
        |> Vector2d.per Physics2d.Time.step


setVelocity : Vector2d.Vector2d Speed.MetersPerSecond TopLeft -> Object -> Object
setVelocity newVelocity (Object internals) =
    let
        displacement : Vector2d.Vector2d Length.Meters TopLeft
        displacement =
            Vector2d.reverse newVelocity
                |> Vector2d.for Physics2d.Time.step
    in
    Object
        { internals
            | positionPrevious =
                internals.position
                    |> Point2d.translateBy displacement
        }


addVelocity : Vector2d.Vector2d Speed.MetersPerSecond TopLeft -> Object -> Object
addVelocity velocityToAdd (Object internals) =
    let
        displacement : Vector2d.Vector2d Length.Meters TopLeft
        displacement =
            Vector2d.reverse velocityToAdd
                |> Vector2d.for Physics2d.Time.step
    in
    Object
        { internals
            | positionPrevious =
                internals.positionPrevious
                    |> Point2d.translateBy displacement
        }


setRotation : Angle.Angle -> Object -> Object
setRotation newRotation (Object internals) =
    Object
        { internals
            | rotation = newRotation
            , rotationPrevious = newRotation
        }


integrate : Object -> Object
integrate (Object internals) =
    let
        angularSpeed : AngularSpeed.AngularSpeed
        angularSpeed =
            AngularSpeed.turnsPerSecond
                (Quantity.minus
                    internals.rotationPrevious
                    internals.rotation
                    |> Angle.inTurns
                )

        rotationStep : Angle.Angle
        rotationStep =
            Angle.turns
                (AngularSpeed.inTurnsPerSecond angularSpeed)

        positionStep : Vector2d.Vector2d Length.Meters TopLeft
        positionStep =
            Vector2d.from internals.positionPrevious internals.position
    in
    Object
        { internals
            | rotation =
                internals.rotation
                    |> Quantity.plus rotationStep
            , rotationPrevious =
                internals.rotation
            , position =
                internals.position
                    |> Point2d.translateBy positionStep
            , positionPrevious =
                internals.position
        }


type alias View =
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , shape : ShapeView
    }


type ShapeView
    = PolygonShapeView (List (Point2d.Point2d Length.Meters TopLeft))
    | CircleShapeView
        { radius : Length.Length
        , position : Point2d.Point2d Length.Meters TopLeft
        }


view : Object -> View
view (Object internals) =
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

module Physics2d.Body exposing
    ( Body
    , fromPolygon, fromCircle
    , update
    , setVelocity, addVelocity
    , ShapeView(..)
    , view, View
    )

{-|

@docs Body
@docs fromPolygon, fromCircle
@docs update
@docs setVelocity, addVelocity
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
import Vector2d


type Body
    = Body Internals


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


fromPolygon :
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , polygon : Physics2d.Polygon.Polygon
    }
    -> Body
fromPolygon { position, rotation, polygon } =
    initialInternals
        { position = position
        , rotation = rotation
        }
        (PolygonShape polygon)
        |> Body


fromCircle :
    { position : Point2d.Point2d Length.Meters TopLeft
    , rotation : Angle.Angle
    , radius : Length.Length
    }
    -> Body
fromCircle { position, rotation, radius } =
    initialInternals
        { position = position
        , rotation = rotation
        }
        (CircleShape
            (Physics2d.Circle.new
                { radius = radius }
            )
        )
        |> Body


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


setVelocity : Vector2d.Vector2d Length.Meters TopLeft -> Body -> Body
setVelocity newVelocity (Body internals) =
    Body
        { internals
            | positionPrevious =
                internals.position
                    |> Point2d.translateBy (Vector2d.reverse newVelocity)
        }


addVelocity : Vector2d.Vector2d Length.Meters TopLeft -> Body -> Body
addVelocity velocityToAdd (Body internals) =
    Body
        { internals
            | positionPrevious =
                internals.positionPrevious
                    |> Point2d.translateBy (Vector2d.reverse velocityToAdd)
        }


update : Body -> Body
update ((Body internals) as body) =
    let
        rotationStep =
            Angle.turns
                (AngularSpeed.inTurnsPerSecond
                    (angularSpeed body)
                )

        positionStep =
            Vector2d.from internals.positionPrevious internals.position
    in
    Body
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


angularSpeed : Body -> AngularSpeed.AngularSpeed
angularSpeed (Body internals) =
    AngularSpeed.turnsPerSecond
        (Quantity.minus
            internals.rotationPrevious
            internals.rotation
            |> Angle.inTurns
        )


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

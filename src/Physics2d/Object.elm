module Physics2d.Object exposing
    ( Object
    , fromPolygon, fromCircle
    , position, setPosition
    , velocity, setVelocity, addVelocity
    , heading, setHeading
    , angularSpeed, setAngularSpeed
    , age
    , integrate
    , areColliding
    , ShapeView(..)
    , view, View
    )

{-|

@docs Object


# Constructors

@docs fromPolygon, fromCircle


# Motion


## Linear

@docs position, setPosition
@docs velocity, setVelocity, addVelocity


## Angular

@docs heading, setHeading
@docs angularSpeed, setAngularSpeed


# Age

@docs age


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
import Axis2d
import Direction2d
import Duration
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
    , heading : Direction2d.Direction2d TopLeft
    , headingPrevious : Direction2d.Direction2d TopLeft
    , age : Duration.Duration
    }


areColliding : Object -> Object -> Bool
areColliding (Object internals1) (Object internals2) =
    case ( internals1.shape, internals2.shape ) of
        ( CircleShape circle1, CircleShape circle2 ) ->
            circlesAreColliding ( internals1, circle1 ) ( internals2, circle2 )

        ( PolygonShape polygon1, PolygonShape polygon2 ) ->
            polygonsAreColliding ( internals1, polygon1 ) ( internals2, polygon2 )

        ( PolygonShape polygon, CircleShape circle ) ->
            polygonAndCircleAreColliding ( internals1, polygon ) ( internals2, circle )

        ( CircleShape circle, PolygonShape polygon ) ->
            polygonAndCircleAreColliding ( internals2, polygon ) ( internals1, circle )


circlesAreColliding :
    ( Internals, Physics2d.Circle.Circle )
    -> ( Internals, Physics2d.Circle.Circle )
    -> Bool
circlesAreColliding ( internals1, circle1 ) ( internals2, circle2 ) =
    let
        distanceBetweenCenters =
            Point2d.distanceFrom
                internals1.position
                internals2.position

        radiusSum =
            Quantity.sum
                [ Physics2d.Circle.radius circle1
                , Physics2d.Circle.radius circle2
                ]
    in
    Quantity.lessThanOrEqualTo radiusSum distanceBetweenCenters


polygonsAreColliding :
    ( Internals, Physics2d.Polygon.Polygon )
    -> ( Internals, Physics2d.Polygon.Polygon )
    -> Bool
polygonsAreColliding ( internals1, polygon1 ) ( internals2, polygon2 ) =
    let
        -- Separating Axis Theorem tests whether there is an axis that separates
        -- two polygons, testing a finite set of axes made from the normals of
        -- each side of each polygon.
        --
        -- 1. Get axes
        -- get all sides of each polygon
        polygon1Sides : List (LineSegment2d.LineSegment2d Length.Meters TopLeft)
        polygon1Sides =
            Physics2d.Polygon.toLineSegments
                { position = internals1.position
                , heading = internals1.heading
                }
                polygon1

        polygon2Sides : List (LineSegment2d.LineSegment2d Length.Meters TopLeft)
        polygon2Sides =
            Physics2d.Polygon.toLineSegments
                { position = internals2.position
                , heading = internals2.heading
                }
                polygon2

        allSides : List (LineSegment2d.LineSegment2d Length.Meters TopLeft)
        allSides =
            polygon1Sides ++ polygon2Sides

        allAxes : List (Axis2d.Axis2d Length.Meters TopLeft)
        allAxes =
            allSides
                -- get the normal of each side
                |> List.filterMap LineSegment2d.perpendicularDirection
                |> List.map
                    (\direction ->
                        -- create an axis from each normal
                        Axis2d.withDirection direction Point2d.origin
                    )

        -- 2. Test separation on each axis
        -- get all vertices of both polygons
        polygon1Vertices : List (Point2d.Point2d Length.Meters TopLeft)
        polygon1Vertices =
            Physics2d.Polygon.toPoints
                { position = internals1.position
                , heading = internals1.heading
                }
                polygon1

        polygon2Vertices : List (Point2d.Point2d Length.Meters TopLeft)
        polygon2Vertices =
            Physics2d.Polygon.toPoints
                { position = internals2.position
                , heading = internals2.heading
                }
                polygon2

        allVertices : List (Point2d.Point2d Length.Meters TopLeft)
        allVertices =
            polygon1Vertices ++ polygon2Vertices

        projectedMinAndMax :
            Axis2d.Axis2d Length.Meters TopLeft
            -> List (Point2d.Point2d Length.Meters TopLeft)
            ->
                Maybe
                    { min : Quantity.Quantity Float Length.Meters
                    , max : Quantity.Quantity Float Length.Meters
                    }
        projectedMinAndMax axis points =
            let
                allPointsProjected : List (Quantity.Quantity Float Length.Meters)
                allPointsProjected =
                    -- project vertices onto the axis
                    points
                        |> List.map (Point2d.signedDistanceAlong axis)

                -- get the min and max for both sets of projected vertices
                maybeMin : Maybe (Quantity.Quantity Float Length.Meters)
                maybeMin =
                    Quantity.minimum allPointsProjected

                maybeMax : Maybe (Quantity.Quantity Float Length.Meters)
                maybeMax =
                    Quantity.maximum allPointsProjected
            in
            Maybe.map2
                (\min max ->
                    { min = min
                    , max = max
                    }
                )
                maybeMin
                maybeMax

        projectedMaxAndMinAreSeparate :
            { min : Quantity.Quantity number Length.Meters
            , max : Quantity.Quantity number Length.Meters
            }
            ->
                { min : Quantity.Quantity number Length.Meters
                , max : Quantity.Quantity number Length.Meters
                }
            -> Bool
        projectedMaxAndMinAreSeparate minMax1 minMax2 =
            (minMax2.max |> Quantity.lessThanOrEqualTo minMax1.min)
                || (minMax1.max |> Quantity.lessThanOrEqualTo minMax2.min)

        areSeparableForAxis :
            Axis2d.Axis2d Length.Meters TopLeft
            -> Maybe Bool
        areSeparableForAxis axis =
            let
                maybeMinMax1 =
                    projectedMinAndMax axis polygon1Vertices

                maybeMinMax2 =
                    projectedMinAndMax axis polygon2Vertices
            in
            Maybe.map2 projectedMaxAndMinAreSeparate
                maybeMinMax1
                maybeMinMax2
    in
    -- if not true for any axis, polygons are colliding
    List.filterMap areSeparableForAxis allAxes
        |> List.any identity
        |> not


polygonAndCircleAreColliding :
    ( Internals, Physics2d.Polygon.Polygon )
    -> ( Internals, Physics2d.Circle.Circle )
    -> Bool
polygonAndCircleAreColliding ( internals1, polygon ) ( internals2, circle ) =
    -- TODO: implement polygon-against-circle collision detection
    False


fromPolygon :
    { position : Point2d.Point2d Length.Meters TopLeft
    , polygon : Physics2d.Polygon.Polygon
    }
    -> Object
fromPolygon config =
    initialInternals
        { position = config.position
        , heading = Direction2d.fromAngle (Angle.turns 0)
        }
        (PolygonShape config.polygon)
        |> Object


fromCircle :
    { position : Point2d.Point2d Length.Meters TopLeft
    , radius : Length.Length
    }
    -> Object
fromCircle config =
    initialInternals
        { position = config.position
        , heading = Direction2d.fromAngle (Angle.turns 0)
        }
        (CircleShape
            (Physics2d.Circle.new
                { radius = config.radius }
            )
        )
        |> Object


initialInternals :
    { position : Point2d.Point2d Length.Meters TopLeft
    , heading : Direction2d.Direction2d TopLeft
    }
    -> shape
    ->
        { position : Point2d.Point2d Length.Meters TopLeft
        , positionPrevious : Point2d.Point2d Length.Meters TopLeft
        , heading : Direction2d.Direction2d TopLeft
        , headingPrevious : Direction2d.Direction2d TopLeft
        , age : Duration.Duration
        , shape : shape
        }
initialInternals config shape =
    { position = config.position
    , positionPrevious = config.position
    , heading = config.heading
    , headingPrevious = config.heading
    , shape = shape
    , age = Duration.seconds 0
    }


position : Object -> Point2d.Point2d Length.Meters TopLeft
position (Object internals) =
    internals.position


setPosition : Point2d.Point2d Length.Meters TopLeft -> Object -> Object
setPosition newPosition (Object internals) =
    let
        displacementToPrevious =
            Vector2d.from internals.position internals.positionPrevious
    in
    Object
        { internals
            | position = newPosition
            , positionPrevious =
                newPosition
                    |> Point2d.translateBy displacementToPrevious
        }


velocity : Object -> Vector2d.Vector2d Speed.MetersPerSecond TopLeft
velocity (Object internals) =
    Vector2d.from internals.positionPrevious internals.position
        |> Vector2d.per Physics2d.Time.step


setVelocity :
    Vector2d.Vector2d Speed.MetersPerSecond TopLeft
    -> Object
    -> Object
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


addVelocity :
    Vector2d.Vector2d Speed.MetersPerSecond TopLeft
    -> Object
    -> Object
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


heading : Object -> Direction2d.Direction2d TopLeft
heading (Object internals) =
    internals.heading


setHeading : Direction2d.Direction2d TopLeft -> Object -> Object
setHeading newHeading (Object internals) =
    Object
        { internals
            | heading = newHeading
            , headingPrevious = newHeading
        }


angularSpeed : Object -> AngularSpeed.AngularSpeed
angularSpeed (Object internals) =
    Direction2d.angleFrom internals.headingPrevious internals.heading
        |> Quantity.per Physics2d.Time.step


setAngularSpeed : AngularSpeed.AngularSpeed -> Object -> Object
setAngularSpeed newAngularSpeed (Object internals) =
    let
        headingStep =
            newAngularSpeed
                |> Quantity.for Physics2d.Time.step

        updatedHeadingPrevious =
            internals.heading
                |> Direction2d.rotateBy
                    (Quantity.negate headingStep)
    in
    Object
        { internals
            | headingPrevious = updatedHeadingPrevious
        }


age : Object -> Duration.Duration
age (Object internals) =
    internals.age


integrate : Object -> Object
integrate (Object internals) =
    let
        headingStep : Angle.Angle
        headingStep =
            angularSpeed (Object internals)
                |> Quantity.for Physics2d.Time.step

        positionStep : Vector2d.Vector2d Length.Meters TopLeft
        positionStep =
            Vector2d.from internals.positionPrevious internals.position
    in
    Object
        { internals
            | heading =
                internals.heading
                    |> Direction2d.rotateBy headingStep
            , headingPrevious =
                internals.heading
            , position =
                internals.position
                    |> Point2d.translateBy positionStep
            , positionPrevious =
                internals.position
            , age =
                Quantity.plus internals.age Physics2d.Time.step
        }


type alias View =
    { position : Point2d.Point2d Length.Meters TopLeft
    , heading : Angle.Angle
    , shape : ShapeView
    }


type ShapeView
    = PolygonShapeView
        { vertices : List (Point2d.Point2d Length.Meters TopLeft)
        }
    | CircleShapeView
        { radius : Length.Length
        , position : Point2d.Point2d Length.Meters TopLeft
        }


view : Object -> View
view (Object internals) =
    { position = internals.position
    , heading = Direction2d.toAngle internals.heading
    , shape = toShapeView internals
    }


toShapeView : Internals -> ShapeView
toShapeView internals =
    case internals.shape of
        PolygonShape polygon ->
            PolygonShapeView
                { vertices =
                    Physics2d.Polygon.toPoints
                        { position = internals.position
                        , heading = internals.heading
                        }
                        polygon
                }

        CircleShape circle ->
            CircleShapeView
                { radius = Physics2d.Circle.radius circle
                , position = internals.position
                }

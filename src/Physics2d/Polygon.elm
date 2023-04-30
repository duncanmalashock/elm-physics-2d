module Physics2d.Polygon exposing
    ( Polygon
    , triangle, square, pentagon, hexagon
    , regular, custom
    , toPoints, toLineSegments
    )

{-|

@docs Polygon
@docs triangle, square, pentagon, hexagon
@docs regular, custom
@docs toPoints, toLineSegments

-}

import Angle
import Direction2d
import Length
import LineSegment2d
import Physics2d.CoordinateSystem exposing (TopLeft)
import Point2d
import Polygon2d
import Quantity
import Vector2d


type Polygon
    = Polygon Internals


type alias Internals =
    { polygon : Polygon2d.Polygon2d Length.Meters TopLeft
    }


triangle : { radius : Length.Length } -> Polygon
triangle { radius } =
    regular { sides = 3, radius = radius }


square : { radius : Length.Length } -> Polygon
square { radius } =
    regular { sides = 4, radius = radius }


pentagon : { radius : Length.Length } -> Polygon
pentagon { radius } =
    regular { sides = 5, radius = radius }


hexagon : { radius : Length.Length } -> Polygon
hexagon { radius } =
    regular { sides = 6, radius = radius }


regular : { sides : Int, radius : Length.Length } -> Polygon
regular { sides, radius } =
    let
        anglePerSide : Angle.Angle
        anglePerSide =
            Angle.turns (1 / Basics.toFloat sides)

        squareAngleAdjustment : Float
        squareAngleAdjustment =
            if sides == 4 then
                0.125

            else
                0

        angles : List Angle.Angle
        angles =
            List.range 0 (sides - 1)
                |> List.map Basics.toFloat
                |> List.map
                    (\number ->
                        Quantity.multiplyBy number anglePerSide
                            |> Quantity.plus
                                (Angle.turns squareAngleAdjustment)
                    )

        toVertex : Angle.Angle -> Point2d.Point2d Length.Meters TopLeft
        toVertex angle =
            Point2d.xy
                (Quantity.multiplyBy (Angle.cos angle) radius)
                (Quantity.multiplyBy (Angle.sin angle) radius)

        vertices : List (Point2d.Point2d Length.Meters TopLeft)
        vertices =
            angles
                |> List.map toVertex
    in
    Polygon
        { polygon = Polygon2d.singleLoop vertices
        }


custom : { vertices : List (Point2d.Point2d Length.Meters TopLeft) } -> Polygon
custom { vertices } =
    Polygon { polygon = Polygon2d.singleLoop vertices }


toPoints :
    { position : Point2d.Point2d Length.Meters TopLeft
    , heading : Direction2d.Direction2d TopLeft
    }
    -> Polygon
    -> List (Point2d.Point2d Length.Meters TopLeft)
toPoints { position, heading } (Polygon { polygon }) =
    polygon
        |> Polygon2d.vertices
        |> List.map
            (Point2d.rotateAround
                Point2d.origin
                (Direction2d.toAngle heading)
            )
        |> List.map
            (Point2d.translateBy
                (Vector2d.from Point2d.origin position)
            )


toLineSegments :
    { position : Point2d.Point2d Length.Meters TopLeft
    , heading : Direction2d.Direction2d TopLeft
    }
    -> Polygon
    -> List (LineSegment2d.LineSegment2d Length.Meters TopLeft)
toLineSegments { position, heading } (Polygon { polygon }) =
    polygon
        |> Polygon2d.edges
        |> List.map
            (LineSegment2d.rotateAround
                Point2d.origin
                (Direction2d.toAngle heading)
            )
        |> List.map
            (LineSegment2d.translateBy
                (Vector2d.from Point2d.origin position)
            )

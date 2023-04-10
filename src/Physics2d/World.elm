module Physics2d.World exposing
    ( World
    , init
    , addBody
    , update
    , viewSvg
    )

{-|

@docs World
@docs init
@docs addBody
@docs update
@docs viewSvg

-}

import Angle
import AssocList as Dict exposing (Dict)
import Circle2d
import Frame2d
import Geometry.Svg
import Html
import Html.Attributes
import Length
import LineSegment2d
import Physics2d.Body
import Physics2d.CoordinateSystem exposing (TopLeft)
import Physics2d.Polygon
import Pixels
import Point2d
import Polygon2d
import Quantity
import Svg
import Svg.Attributes


type World bodyId
    = World (Internals bodyId)


type alias Internals bodyId =
    { dimensions :
        { x : Length.Length
        , y : Length.Length
        }
    , bodies : Dict bodyId Physics2d.Body.Body
    }


init :
    { width : Length.Length
    , height : Length.Length
    }
    -> World bodyId
init { width, height } =
    World
        { dimensions =
            { x = width
            , y = height
            }
        , bodies = Dict.empty
        }


addBody : bodyId -> Physics2d.Body.Body -> World bodyId -> World bodyId
addBody bodyId body (World internals) =
    World
        { internals
            | bodies = Dict.insert bodyId body internals.bodies
        }


update : World bodyId -> World bodyId
update (World internals) =
    World
        { internals
            | bodies =
                Dict.map (\id -> Physics2d.Body.update) internals.bodies
        }


viewSvg :
    { widthInPixels : Float, heightInPixels : Float }
    -> World bodyId
    -> Html.Html msg
viewSvg { widthInPixels, heightInPixels } (World { dimensions, bodies }) =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 heightInPixels)
                |> Frame2d.reverseY

        svgOutput : List (Svg.Svg msg)
        svgOutput =
            bodies
                |> Dict.values
                |> List.map Physics2d.Body.view
                |> List.concatMap bodyViewToSvg

        bodyViewToSvg : Physics2d.Body.View -> List (Svg.Svg msg)
        bodyViewToSvg body =
            case body.shape of
                Physics2d.Body.PolygonShapeView vertices ->
                    [ polygonVerticesToSvg vertices ]

                Physics2d.Body.CircleShapeView circleShapeView ->
                    [ circleShapeViewToSvg circleShapeView ]

        polygonVerticesToSvg :
            List (Point2d.Point2d Length.Meters TopLeft)
            -> Svg.Svg msg
        polygonVerticesToSvg vertices =
            Geometry.Svg.polygon2d
                [ Svg.Attributes.fill "#282828"
                ]
                (Polygon2d.singleLoop vertices)

        circleShapeViewToSvg :
            { radius : Length.Length
            , position : Point2d.Point2d Length.Meters TopLeft
            }
            -> Svg.Svg msg
        circleShapeViewToSvg circleShapeView =
            Geometry.Svg.circle2d
                [ Svg.Attributes.fill "#282828"
                ]
                (Circle2d.withRadius circleShapeView.radius
                    circleShapeView.position
                )

        pixelsPerMeter =
            Pixels.pixels 10
                |> Quantity.per (Length.meters 1)
    in
    Svg.svg
        [ Svg.Attributes.width (String.fromFloat widthInPixels)
        , Svg.Attributes.height (String.fromFloat widthInPixels)
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "background" "#f8f8f8"
        ]
        (svgOutput
            |> List.map (Geometry.Svg.at pixelsPerMeter)
            |> List.map (Geometry.Svg.relativeTo topLeftFrame)
        )

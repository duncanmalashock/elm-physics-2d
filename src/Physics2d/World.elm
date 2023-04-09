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


type World
    = World Internals


type alias Internals =
    { dimensions :
        { x : Length.Length
        , y : Length.Length
        }
    , bodies : List Physics2d.Body.Body
    }


init :
    { width : Length.Length
    , height : Length.Length
    }
    -> World
init { width, height } =
    World
        { dimensions =
            { x = width
            , y = height
            }
        , bodies = []
        }


addBody : Physics2d.Body.Body -> World -> World
addBody body (World internals) =
    World
        { internals
            | bodies = body :: internals.bodies
        }


update : World -> World
update world =
    world


viewSvg :
    { widthInPixels : Float, heightInPixels : Float }
    -> World
    -> Html.Html msg
viewSvg { widthInPixels, heightInPixels } (World { dimensions, bodies }) =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 heightInPixels)
                |> Frame2d.reverseY

        svgOutput : List (Svg.Svg msg)
        svgOutput =
            List.map Physics2d.Body.view bodies
                |> List.concatMap bodyViewToSvg

        bodyViewToSvg : Physics2d.Body.View -> List (Svg.Svg msg)
        bodyViewToSvg body =
            case body.shape of
                Physics2d.Body.PolygonShapeView vertices ->
                    [ polygonVerticesToSvg vertices ]

        polygonVerticesToSvg :
            List (Point2d.Point2d Length.Meters TopLeft)
            -> Svg.Svg msg
        polygonVerticesToSvg vertices =
            Geometry.Svg.polygon2d
                [ Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "0.1"
                , Svg.Attributes.fill "white"
                ]
                (Polygon2d.singleLoop vertices)

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

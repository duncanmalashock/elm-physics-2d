module Physics2d.World exposing
    ( World
    , init
    , addObject
    , update
    , viewSvg
    )

{-|

@docs World
@docs init
@docs addObject
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
import Physics2d.CoordinateSystem exposing (TopLeft)
import Physics2d.Object
import Physics2d.Polygon
import Pixels
import Point2d
import Polygon2d
import Quantity
import Svg
import Svg.Attributes


type World objectId
    = World (Internals objectId)


type alias Internals objectId =
    { dimensions :
        { x : Length.Length
        , y : Length.Length
        }
    , objects : Dict objectId Physics2d.Object.Object
    }


init :
    { width : Length.Length
    , height : Length.Length
    }
    -> World objectId
init { width, height } =
    World
        { dimensions =
            { x = width
            , y = height
            }
        , objects = Dict.empty
        }


addObject : objectId -> Physics2d.Object.Object -> World objectId -> World objectId
addObject objectId object (World internals) =
    World
        { internals
            | objects = Dict.insert objectId object internals.objects
        }


update : World objectId -> World objectId
update (World internals) =
    World
        { internals
            | objects =
                Dict.map (\id -> Physics2d.Object.update) internals.objects
        }


viewSvg :
    { widthInPixels : Float, heightInPixels : Float }
    -> World objectId
    -> Html.Html msg
viewSvg { widthInPixels, heightInPixels } (World { dimensions, objects }) =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 heightInPixels)
                |> Frame2d.reverseY

        svgOutput : List (Svg.Svg msg)
        svgOutput =
            objects
                |> Dict.values
                |> List.map Physics2d.Object.view
                |> List.concatMap objectViewToSvg

        objectViewToSvg : Physics2d.Object.View -> List (Svg.Svg msg)
        objectViewToSvg object =
            case object.shape of
                Physics2d.Object.PolygonShapeView vertices ->
                    [ polygonVerticesToSvg vertices ]

                Physics2d.Object.CircleShapeView circleShapeView ->
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

module Pages.Home_ exposing (Model, Msg, page)

import Angle
import Browser.Events
import Circle2d
import Duration
import Effect exposing (Effect)
import Frame2d
import Geometry.Svg
import Html
import Html.Attributes
import Length
import Page exposing (Page)
import Physics2d.CoordinateSystem exposing (TopLeft)
import Physics2d.Object
import Physics2d.Polygon
import Physics2d.World
import Pixels
import Point2d
import Polygon2d
import Quantity
import Route
import Shared
import Svg
import Svg.Attributes
import Time
import Vector2d
import View exposing (View)


page : Shared.Model -> Route.Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view route
        }


type alias Model =
    { world : Physics2d.World.World ObjectId
    }


type ObjectId
    = Circle Int


init : () -> ( Model, Effect Msg )
init () =
    let
        circle1 : Physics2d.Object.Object
        circle1 =
            Physics2d.Object.fromCircle
                { position =
                    Point2d.xy (Length.meters 10) (Length.meters 30)
                , radius = Length.meters 1
                }
                |> Physics2d.Object.setVelocity
                    (Vector2d.xy (Length.meters 10) (Length.meters 10)
                        |> Vector2d.per Duration.second
                    )

        circle2 : Physics2d.Object.Object
        circle2 =
            Physics2d.Object.fromCircle
                { position =
                    Point2d.xy (Length.meters 50) (Length.meters 30)
                , radius = Length.meters 1
                }
                |> Physics2d.Object.setVelocity
                    (Vector2d.xy (Length.meters -10) (Length.meters 10)
                        |> Vector2d.per Duration.second
                    )
    in
    ( { world =
            Physics2d.World.init
                { height = Length.meters 300
                , width = Length.meters 300
                }
                |> Physics2d.World.addObject
                    { id = Circle 1
                    , object = circle1
                    }
                |> Physics2d.World.addObject
                    { id = Circle 2
                    , object = circle2
                    }
      }
    , Effect.none
    )


type Msg
    = UpdateFrame
    | CirclesCollided ( ObjectId, Physics2d.Object.Object ) ( ObjectId, Physics2d.Object.Object )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateFrame ->
            let
                ( updatedWorld, cmd ) =
                    Physics2d.World.update
                        { rules =
                            [ applyGravity
                            ]
                        , collisionHandlers =
                            [ ( areBothCircles, CirclesCollided )
                            ]
                        , world = model.world
                        }
            in
            ( { model
                | world =
                    updatedWorld
              }
            , Effect.sendCmd cmd
            )

        CirclesCollided ( id1, object1 ) ( id2, object2 ) ->
            ( { model
                | world =
                    model.world
                        |> Physics2d.World.removeObject id1
                        |> Physics2d.World.removeObject id2
              }
            , Effect.none
            )


areBothCircles : ObjectId -> ObjectId -> Bool
areBothCircles id1 id2 =
    case ( id1, id2 ) of
        ( Circle _, Circle _ ) ->
            True


applyGravity :
    ObjectId
    -> Physics2d.World.World ObjectId
    -> Physics2d.Object.Object
    -> Physics2d.Object.Object
applyGravity id world object =
    object
        |> Physics2d.Object.addVelocity
            (Vector2d.xy
                (Length.meters 0)
                (Length.meters -0.1)
                |> Vector2d.per Duration.second
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta (\timeElapsed -> UpdateFrame)


view : Route.Route () -> Model -> View msg
view route model =
    { title = "Physics2D"
    , body =
        [ viewSvg
            { widthInPixels = 600, heightInPixels = 600 }
            model.world
        ]
    }


viewSvg :
    { widthInPixels : Float, heightInPixels : Float }
    -> Physics2d.World.World objectId
    -> Html.Html msg
viewSvg { widthInPixels, heightInPixels } world =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 heightInPixels)
                |> Frame2d.reverseY

        svgOutput : List (Svg.Svg msg)
        svgOutput =
            Physics2d.World.objectViews world
                |> List.concatMap objectViewToSvg

        objectViewToSvg : Physics2d.Object.View -> List (Svg.Svg msg)
        objectViewToSvg object =
            case object.shape of
                Physics2d.Object.PolygonShapeView polygonShapeView ->
                    [ Geometry.Svg.polygon2d
                        [ Svg.Attributes.fill "#282828" ]
                        (Polygon2d.singleLoop polygonShapeView.vertices)
                    ]

                Physics2d.Object.CircleShapeView circleShapeView ->
                    [ Geometry.Svg.circle2d
                        [ Svg.Attributes.fill "#282828" ]
                        (Circle2d.withRadius circleShapeView.radius
                            circleShapeView.position
                        )
                    ]

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

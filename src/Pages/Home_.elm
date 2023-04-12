module Pages.Home_ exposing (Model, Msg, page)

import AngularSpeed
import AssocSet
import Browser.Events
import Circle2d
import Duration
import Effect exposing (Effect)
import Frame2d
import Geometry.Svg
import Html
import Html.Attributes
import Json.Decode
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
import Speed
import Svg
import Svg.Attributes
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
    , keys : AssocSet.Set Key
    }


type Key
    = LeftArrow
    | RightArrow
    | UpArrow
    | DownArrow
    | Spacebar


type ObjectId
    = PlayerShip


init : () -> ( Model, Effect Msg )
init () =
    let
        playerShip : Physics2d.Object.Object
        playerShip =
            Physics2d.Object.fromPolygon
                { position =
                    Point2d.xy (Length.meters 30) (Length.meters 30)
                , polygon =
                    Physics2d.Polygon.custom
                        { vertices =
                            [ Point2d.xy (Length.meters -1) (Length.meters -1)
                            , Point2d.xy (Length.meters -1) (Length.meters 1)
                            , Point2d.xy (Length.meters 2) (Length.meters 0)
                            ]
                        }
                }
    in
    ( { world =
            Physics2d.World.init
                { height = Length.meters 300
                , width = Length.meters 300
                , objects =
                    [ ( PlayerShip, playerShip )
                    ]
                }
      , keys = AssocSet.empty
      }
    , Effect.none
    )


type Msg
    = UpdateFrame
    | KeyUp Key
    | KeyDown Key


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateFrame ->
            let
                ( updatedWorld, cmd ) =
                    Physics2d.World.update
                        { rules =
                            [ updatePlayerShip model.keys
                            , wrapAround
                            ]
                        , collisionHandlers = []
                        , world = model.world
                        }
            in
            ( { model
                | world =
                    updatedWorld
              }
            , Effect.sendCmd cmd
            )

        KeyDown key ->
            ( { model
                | keys = AssocSet.insert key model.keys
              }
            , Effect.none
            )

        KeyUp key ->
            ( { model
                | keys = AssocSet.remove key model.keys
              }
            , Effect.none
            )


updatePlayerShip :
    AssocSet.Set Key
    -> ObjectId
    -> Physics2d.World.World ObjectId
    -> Physics2d.Object.Object
    -> Physics2d.Object.Object
updatePlayerShip keys objectId world object =
    if objectId == PlayerShip then
        let
            turnKeyMultiplier : number
            turnKeyMultiplier =
                List.sum
                    [ if AssocSet.member LeftArrow keys then
                        1

                      else
                        0
                    , if AssocSet.member RightArrow keys then
                        -1

                      else
                        0
                    ]

            angularSpeed : AngularSpeed.AngularSpeed
            angularSpeed =
                AngularSpeed.turnsPerSecond 0.4
                    |> Quantity.multiplyBy turnKeyMultiplier

            velocityToAdd : Vector2d.Vector2d Speed.MetersPerSecond TopLeft
            velocityToAdd =
                if AssocSet.member UpArrow keys then
                    Vector2d.withLength (Length.meters 0.125)
                        (Physics2d.Object.heading object)
                        |> Vector2d.per Duration.second

                else
                    Vector2d.zero
        in
        object
            |> Physics2d.Object.setAngularSpeed angularSpeed
            |> Physics2d.Object.addVelocity velocityToAdd

    else
        object


wrapAround :
    ObjectId
    -> Physics2d.World.World ObjectId
    -> Physics2d.Object.Object
    -> Physics2d.Object.Object
wrapAround objectId world object =
    let
        { x, y } =
            Physics2d.Object.position object
                |> Point2d.toRecord Length.inMeters

        newPosition =
            Physics2d.Object.position object
                |> (if x > 60 then
                        Point2d.translateBy
                            (Vector2d.xy (Length.meters -60) (Length.meters 0))

                    else
                        identity
                   )
                |> (if x < 0 then
                        Point2d.translateBy
                            (Vector2d.xy (Length.meters 60) (Length.meters 0))

                    else
                        identity
                   )
                |> (if y > 60 then
                        Point2d.translateBy
                            (Vector2d.xy (Length.meters 0) (Length.meters -60))

                    else
                        identity
                   )
                |> (if y < 0 then
                        Point2d.translateBy
                            (Vector2d.xy (Length.meters 0) (Length.meters 60))

                    else
                        identity
                   )
    in
    Physics2d.Object.setPosition newPosition object


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (\timeElapsed -> UpdateFrame)
        , Browser.Events.onKeyDown (keyEventDecoder KeyDown)
        , Browser.Events.onKeyUp (keyEventDecoder KeyUp)
        ]


keyEventDecoder : (Key -> Msg) -> Json.Decode.Decoder Msg
keyEventDecoder toMsg =
    let
        toKey : String -> Json.Decode.Decoder Key
        toKey code =
            case code of
                "ArrowUp" ->
                    Json.Decode.succeed UpArrow

                "ArrowDown" ->
                    Json.Decode.succeed DownArrow

                "ArrowLeft" ->
                    Json.Decode.succeed LeftArrow

                "ArrowRight" ->
                    Json.Decode.succeed RightArrow

                "Space" ->
                    Json.Decode.succeed Spacebar

                _ ->
                    Json.Decode.fail "Another key was pressed"
    in
    Json.Decode.field "code" Json.Decode.string
        |> Json.Decode.andThen toKey
        |> Json.Decode.map toMsg


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
                        [ Svg.Attributes.fill "#f8f8f8" ]
                        (Polygon2d.singleLoop polygonShapeView.vertices)
                    ]

                Physics2d.Object.CircleShapeView circleShapeView ->
                    [ Geometry.Svg.circle2d
                        [ Svg.Attributes.fill "#f8f8f8" ]
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
        , Html.Attributes.style "background" "#181818"
        ]
        (svgOutput
            |> List.map (Geometry.Svg.at pixelsPerMeter)
            |> List.map (Geometry.Svg.relativeTo topLeftFrame)
        )

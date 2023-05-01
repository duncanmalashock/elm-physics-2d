module Pages.Home_ exposing (Model, Msg, page)

import Angle
import AngularSpeed
import AssocSet
import Browser.Events
import Circle2d
import Direction2d
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
import Random
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
    { world : Physics2d.World.World ObjectGroup
    , keys : AssocSet.Set Key
    , playerIsFiring : Bool
    , shouldCreateNewAsteroids : Bool
    }


type Key
    = LeftArrow
    | RightArrow
    | UpArrow
    | DownArrow
    | Spacebar


type ObjectGroup
    = PlayerShip
    | PlayerBullet
    | Asteroid


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
                |> Physics2d.Object.setHeading
                    (Angle.turns 0.25 |> Direction2d.fromAngle)
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
      , playerIsFiring = False
      , shouldCreateNewAsteroids = True
      }
    , Effect.none
    )


type Msg
    = UpdateFrame
    | KeyUp Key
    | KeyDown Key
    | NewAsteroidAngleGenerated Float
    | PlayerBulletHitAsteroid (Physics2d.World.Collision ObjectGroup)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateFrame ->
            let
                updatedWorld : Physics2d.World.World ObjectGroup
                updatedWorld =
                    model.world
                        |> Physics2d.World.updateGroups [ PlayerShip ]
                            (updatePlayerShip model.keys)
                        |> Physics2d.World.updateAll wrapAround
                        |> Physics2d.World.removeObjectIf [ PlayerBullet ]
                            bulletIsPastMaxDuration
                        |> createPlayerBullet model.playerIsFiring
                        |> Physics2d.World.simulate
            in
            ( { model
                | world = updatedWorld
                , playerIsFiring = False
              }
            , Cmd.batch
                [ createNewAsteroids model.shouldCreateNewAsteroids
                , Physics2d.World.onOverlap
                    { msg = PlayerBulletHitAsteroid
                    , betweenGroups = ( Asteroid, PlayerBullet )
                    , world = updatedWorld
                    }
                ]
                |> Effect.sendCmd
            )

        KeyDown key ->
            ( { model
                | keys = AssocSet.insert key model.keys
                , playerIsFiring = key == Spacebar
              }
            , Effect.none
            )

        KeyUp key ->
            ( { model
                | keys = AssocSet.remove key model.keys
              }
            , Effect.none
            )

        NewAsteroidAngleGenerated angleTurns ->
            let
                newVector =
                    Vector2d.rTheta (Length.meters 60) (Angle.turns angleTurns)

                newPosition =
                    Point2d.xy (Length.meters 30) (Length.meters 30)
                        |> Point2d.translateBy newVector
                        |> Point2d.toTuple Length.inMeters
                        |> (\( x, y ) ->
                                Point2d.fromTuple Length.meters
                                    ( Basics.clamp 0 60 x
                                    , Basics.clamp 0 60 y
                                    )
                           )

                initialVelocity =
                    Vector2d.withLength (Length.meters 1)
                        (Angle.turns (angleTurns + 0.5)
                            |> Direction2d.fromAngle
                        )
                        |> Vector2d.per Duration.second

                initialAngularSpeed =
                    Angle.turns 0.05
                        |> Quantity.per Duration.second

                newAsteroid : Physics2d.Object.Object
                newAsteroid =
                    Physics2d.Object.fromPolygon
                        { position = newPosition
                        , polygon =
                            Physics2d.Polygon.hexagon
                                { radius = Length.meters 3
                                }
                        }
                        |> Physics2d.Object.setVelocity initialVelocity
                        |> Physics2d.Object.setAngularSpeed initialAngularSpeed
            in
            ( { model
                | shouldCreateNewAsteroids = False
                , world =
                    model.world
                        |> Physics2d.World.addObject
                            ( Asteroid, newAsteroid )
              }
            , Effect.none
            )

        PlayerBulletHitAsteroid { objectA, objectB } ->
            ( { model
                | world =
                    model.world
                        |> Physics2d.World.removeObject objectA.objectId
                        |> Physics2d.World.removeObject objectB.objectId
              }
            , Effect.none
            )


createPlayerBullet :
    Bool
    -> Physics2d.World.World ObjectGroup
    -> Physics2d.World.World ObjectGroup
createPlayerBullet isFiring world =
    if isFiring then
        Physics2d.World.getObjects [ PlayerShip ] world
            |> List.map
                (\player ->
                    let
                        initialVelocity =
                            Vector2d.withLength (Length.meters 25)
                                (Physics2d.Object.heading player)
                                |> Vector2d.per Duration.second
                                |> Vector2d.plus
                                    (Physics2d.Object.velocity player)

                        newBullet : Physics2d.Object.Object
                        newBullet =
                            Physics2d.Object.fromCircle
                                { position = Physics2d.Object.position player
                                , radius = Length.meters 0.3
                                }
                                |> Physics2d.Object.setVelocity initialVelocity
                    in
                    ( PlayerBullet, newBullet )
                )
            |> List.foldl Physics2d.World.addObject world

    else
        world


createNewAsteroids : Bool -> Cmd Msg
createNewAsteroids shouldCreateNewAsteroids =
    if shouldCreateNewAsteroids then
        List.repeat 6
            (Random.generate NewAsteroidAngleGenerated (Random.float 0 1))
            |> Cmd.batch

    else
        Cmd.none


updatePlayerShip :
    AssocSet.Set Key
    -> Physics2d.Object.Object
    -> Physics2d.Object.Object
updatePlayerShip keys ship =
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
                    (Physics2d.Object.heading ship)
                    |> Vector2d.per Duration.second

            else
                Vector2d.zero
    in
    ship
        |> Physics2d.Object.setAngularSpeed angularSpeed
        |> Physics2d.Object.addVelocity velocityToAdd


wrapAround :
    Physics2d.Object.Object
    -> Physics2d.Object.Object
wrapAround object =
    let
        { x, y } =
            Physics2d.Object.position object
                |> Point2d.toRecord Length.inMeters

        translateX =
            if x > 60 then
                -60

            else if x < 0 then
                60

            else
                0

        translateY =
            if y > 60 then
                -60

            else if y < 0 then
                60

            else
                0

        newPosition =
            Physics2d.Object.position object
                |> Point2d.translateBy
                    (Vector2d.xy
                        (Length.meters translateX)
                        (Length.meters translateY)
                    )
    in
    Physics2d.Object.setPosition newPosition object


bulletIsPastMaxDuration : Physics2d.Object.Object -> Bool
bulletIsPastMaxDuration object =
    let
        bulletMaxDuration =
            Duration.milliseconds 1500
    in
    Quantity.greaterThan bulletMaxDuration
        (Physics2d.Object.age object)


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
    -> Physics2d.World.World ObjectGroup
    -> Html.Html msg
viewSvg { widthInPixels, heightInPixels } world =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 heightInPixels)
                |> Frame2d.reverseY

        svgOutput : List (Svg.Svg msg)
        svgOutput =
            Physics2d.World.viewData world
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

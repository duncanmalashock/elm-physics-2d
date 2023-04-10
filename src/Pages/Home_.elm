module Pages.Home_ exposing (Model, Msg, page)

import Angle
import Browser.Events
import Effect exposing (Effect)
import Html
import Length
import Page exposing (Page)
import Physics2d.Object
import Physics2d.Polygon
import Physics2d.World
import Point2d
import Quantity
import Route
import Shared
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
    = Circle1
    | Circle2


init : () -> ( Model, Effect Msg )
init () =
    ( initialModel, Effect.none )


initialModel : Model
initialModel =
    { world =
        Physics2d.World.init
            { height = Length.meters 300
            , width = Length.meters 300
            }
            |> Physics2d.World.addObject
                Circle1
                (Physics2d.Object.fromCircle
                    { position =
                        Point2d.xy
                            (Length.meters 30)
                            (Length.meters 30)
                    , radius = Length.meters 2.5
                    }
                    |> Physics2d.Object.setVelocity
                        (Vector2d.xy (Length.meters 0) (Length.meters 0.2))
                )
            |> Physics2d.World.addObject
                Circle2
                (Physics2d.Object.fromCircle
                    { position =
                        Point2d.xy
                            (Length.meters 30)
                            (Length.meters 40)
                    , radius = Length.meters 2.5
                    }
                    |> Physics2d.Object.setVelocity
                        (Vector2d.xy (Length.meters 0) (Length.meters 0.2))
                )
    }


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
                            , applyFrictionToCircle1
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
            ( model, Effect.none )


areBothCircles : ObjectId -> ObjectId -> Bool
areBothCircles id1 id2 =
    case ( id1, id2 ) of
        ( Circle1, Circle2 ) ->
            True

        ( Circle2, Circle1 ) ->
            True

        _ ->
            False


applyFrictionToCircle1 :
    ObjectId
    -> Physics2d.World.World ObjectId
    -> Physics2d.Object.Object
    -> Physics2d.Object.Object
applyFrictionToCircle1 id world object =
    case id of
        Circle1 ->
            object
                |> Physics2d.Object.addVelocity
                    (object
                        |> Physics2d.Object.velocity
                        |> Vector2d.reverse
                        |> Vector2d.scaleBy 0.02
                    )

        _ ->
            object


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
                (Length.meters -0.005)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta (\timeElapsed -> UpdateFrame)


view : Route.Route () -> Model -> View msg
view route model =
    { title = "Physics2D"
    , body =
        [ Physics2d.World.viewSvg
            { widthInPixels = 600, heightInPixels = 600 }
            model.world
        ]
    }

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
    = Triangle
    | Circle


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
                Triangle
                (Physics2d.Object.fromPolygon
                    { position =
                        Point2d.xy
                            (Length.meters 30)
                            (Length.meters 30)
                    , rotation = Angle.turns 0
                    , polygon =
                        Physics2d.Polygon.triangle
                            { radius = Length.meters 3
                            }
                    }
                    |> Physics2d.Object.setVelocity
                        (Vector2d.xy (Length.meters 0) (Length.meters 0.2))
                )
            |> Physics2d.World.addObject
                Circle
                (Physics2d.Object.fromCircle
                    { position =
                        Point2d.xy
                            (Length.meters 40)
                            (Length.meters 30.5)
                    , rotation = Angle.turns 0
                    , radius = Length.meters 2.5
                    }
                    |> Physics2d.Object.setVelocity
                        (Vector2d.xy (Length.meters 0) (Length.meters 0.2))
                )
    }


type Msg
    = UpdateFrame


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateFrame ->
            ( { model
                | world =
                    Physics2d.World.update
                        { rules =
                            [ applyFrictionToTriangle
                            ]
                        , world = model.world
                        }
              }
            , Effect.none
            )


applyFrictionToTriangle :
    ObjectId
    -> Physics2d.World.World ObjectId
    -> Physics2d.Object.Object
    -> Physics2d.Object.Object
applyFrictionToTriangle id world object =
    case id of
        Triangle ->
            Physics2d.Object.addVelocity
                (object
                    |> Physics2d.Object.velocity
                    |> Vector2d.reverse
                    |> Vector2d.scaleBy 0.01
                )
                object

        _ ->
            object


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

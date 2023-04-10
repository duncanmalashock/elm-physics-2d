module Pages.Home_ exposing (Model, Msg, page)

import Angle
import Browser.Events
import Effect exposing (Effect)
import Html
import Length
import Page exposing (Page)
import Physics2d.Body
import Physics2d.Polygon
import Physics2d.World
import Point2d
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
    { world : Physics2d.World.World BodyId
    }


type BodyId
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
            |> Physics2d.World.addBody
                Triangle
                (Physics2d.Body.fromPolygon
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
                )
            |> Physics2d.World.addBody
                Circle
                (Physics2d.Body.fromCircle
                    { position =
                        Point2d.xy
                            (Length.meters 40)
                            (Length.meters 31.4)
                    , rotation = Angle.turns 0
                    , radius = Length.meters 2.5
                    }
                )
    }


type Msg
    = UpdateFrame


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateFrame ->
            ( { model
                | world = Physics2d.World.update model.world
              }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta (\timeElapsed -> UpdateFrame)


view : Route.Route () -> Model -> View msg
view route model =
    { title = "Physics2D"
    , body = viewBody model
    }


viewBody : Model -> List (Html.Html msg)
viewBody model =
    [ Physics2d.World.viewSvg
        { widthInPixels = 600, heightInPixels = 600 }
        model.world
    ]

module Pages.Home_ exposing (Model, Msg, page)

import Angle
import Html
import Length
import Page exposing (Page)
import Physics2d.Body
import Physics2d.Polygon
import Physics2d.World
import Point2d
import View exposing (View)


page : Page Model Msg
page =
    Page.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { world : Physics2d.World.World
    }


init : Model
init =
    { world =
        Physics2d.World.init
            { height = Length.meters 300
            , width = Length.meters 300
            }
            |> Physics2d.World.addBody
                (Physics2d.Body.fromPolygon
                    { position =
                        Point2d.xy
                            (Length.meters 30)
                            (Length.meters 30)
                    , rotation = Angle.turns 0
                    , polygon =
                        Physics2d.Polygon.triangle
                            { radius = Length.meters 5
                            }
                    }
                )
    }


type Msg
    = Msg


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> View msg
view model =
    { title = "Physics2D"
    , body = viewBody model
    }


viewBody : Model -> List (Html.Html msg)
viewBody model =
    [ Physics2d.World.viewSvg
        { widthInPixels = 600, heightInPixels = 600 }
        model.world
    ]

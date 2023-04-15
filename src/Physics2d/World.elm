module Physics2d.World exposing
    ( World
    , init
    , addObject, removeObject
    , simulate
    , viewData
    )

{-|

@docs World
@docs init
@docs addObject, removeObject
@docs simulate
@docs viewData

-}

import AssocList as Dict exposing (Dict)
import Length
import List.Extra
import Physics2d.Object
import Random
import Task


type World
    = World Internals


type alias Internals =
    { dimensions :
        { x : Length.Length
        , y : Length.Length
        }
    , objects : Dict ObjectId Physics2d.Object.Object
    , timeSteps : Int
    , objectsCreatedThisStep : Int
    }


type ObjectId
    = Id String


init :
    { width : Length.Length
    , height : Length.Length
    , objects : List Physics2d.Object.Object
    }
    -> World
init { width, height, objects } =
    let
        initialWorld : World
        initialWorld =
            World
                { dimensions =
                    { x = width
                    , y = height
                    }
                , objects = Dict.empty
                , timeSteps = 0
                , objectsCreatedThisStep = 0
                }
    in
    List.foldl addObject initialWorld objects


addObject : Physics2d.Object.Object -> World -> World
addObject object (World internals) =
    let
        newObjectId : ObjectId
        newObjectId =
            String.fromInt internals.timeSteps
                ++ "-"
                ++ String.fromInt internals.objectsCreatedThisStep
                |> Id
    in
    World
        { internals
            | objects = Dict.insert newObjectId object internals.objects
            , objectsCreatedThisStep = internals.objectsCreatedThisStep + 1
        }


removeObject : ObjectId -> Physics2d.Object.Object -> World -> World
removeObject id object (World internals) =
    World
        { internals
            | objects = Dict.remove id internals.objects
        }


simulate : World -> World
simulate (World internals) =
    World
        { internals
            | objects =
                internals.objects
                    |> Dict.map (\id -> Physics2d.Object.integrate)
            , timeSteps = internals.timeSteps + 1
            , objectsCreatedThisStep = 0
        }


viewData : World -> List Physics2d.Object.View
viewData (World internals) =
    internals.objects
        |> Dict.values
        |> List.map Physics2d.Object.view

module Physics2d.World exposing
    ( World
    , init
    , addObject
    , removeObjectIf
    , updateGroups, updateAll
    , simulate
    , viewData
    , getObjects
    )

{-|

@docs World
@docs init
@docs addObject
@docs removeObjectIf
@docs getObjectsInGroup
@docs updateGroups, updateAll
@docs simulate
@docs viewData

-}

import AssocList as Dict exposing (Dict)
import Length
import List.Extra
import Physics2d.Object
import Random
import Task


type World group
    = World (Internals group)


type alias Internals group =
    { dimensions :
        { x : Length.Length
        , y : Length.Length
        }
    , objects : Dict ObjectId ( group, Physics2d.Object.Object )
    , timeSteps : Int
    , objectsCreatedThisStep : Int
    }


type ObjectId
    = Id String


init :
    { width : Length.Length
    , height : Length.Length
    , objects : List ( group, Physics2d.Object.Object )
    }
    -> World group
init { width, height, objects } =
    let
        initialWorld : World group
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


addObject :
    ( group, Physics2d.Object.Object )
    -> World group
    -> World group
addObject ( group, object ) (World internals) =
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
            | objects =
                Dict.insert
                    newObjectId
                    ( group, object )
                    internals.objects
            , objectsCreatedThisStep = internals.objectsCreatedThisStep + 1
        }


getObjects : List group -> World group -> List Physics2d.Object.Object
getObjects groups (World internals) =
    internals.objects
        |> Dict.filter
            (\id ( group, object ) ->
                List.member group groups
            )
        |> Dict.values
        |> List.map (\( group, object ) -> object)


updateGroups :
    List group
    -> (Physics2d.Object.Object -> Physics2d.Object.Object)
    -> World group
    -> World group
updateGroups groupsToInclude fn (World internals) =
    World
        { internals
            | objects =
                internals.objects
                    |> Dict.map
                        (\id ( group, object ) ->
                            ( group
                            , if List.member group groupsToInclude then
                                fn object

                              else
                                object
                            )
                        )
        }


removeObjectIf :
    List group
    -> (Physics2d.Object.Object -> Bool)
    -> World group
    -> World group
removeObjectIf groupsToInclude predicate (World internals) =
    World
        { internals
            | objects =
                Dict.filter
                    (\id ( group, object ) ->
                        if List.member group groupsToInclude then
                            not (predicate object)

                        else
                            True
                    )
                    internals.objects
        }


updateAll :
    (Physics2d.Object.Object -> Physics2d.Object.Object)
    -> World group
    -> World group
updateAll fn (World internals) =
    World
        { internals
            | objects =
                internals.objects
                    |> Dict.map
                        (\id ( group, object ) ->
                            ( group, fn object )
                        )
        }


simulate : World group -> World group
simulate (World internals) =
    World
        { internals
            | objects =
                internals.objects
                    |> Dict.map
                        (\id ( group, object ) ->
                            ( group, Physics2d.Object.integrate object )
                        )
            , timeSteps = internals.timeSteps + 1
            , objectsCreatedThisStep = 0
        }


viewData : World group -> List Physics2d.Object.View
viewData (World internals) =
    internals.objects
        |> Dict.values
        |> List.map (\( group, object ) -> Physics2d.Object.view object)

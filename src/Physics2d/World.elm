module Physics2d.World exposing
    ( World
    , init
    , addObject, removeObject
    , removeObjectIf
    , getObjects
    , updateGroups, updateAll
    , onOverlap, Collision
    , simulate
    , viewData
    , ObjectId
    )

{-|

@docs World
@docs init
@docs addObject, removeObject
@docs removeObjectIf
@docs getObjects
@docs updateGroups, updateAll
@docs onOverlap, Collision
@docs simulate
@docs viewData
@docs ObjectId

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


removeObject :
    ObjectId
    -> World group
    -> World group
removeObject id (World internals) =
    World
        { internals
            | objects =
                Dict.remove id internals.objects
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


type alias Collision group =
    { objectA :
        { objectId : ObjectId
        , group : group
        , object : Physics2d.Object.Object
        }
    , objectB :
        { objectId : ObjectId
        , group : group
        , object : Physics2d.Object.Object
        }
    }


onOverlap :
    { msg : Collision group -> msg
    , betweenGroups : ( group, group )
    , world : World group
    }
    -> Cmd msg
onOverlap { msg, betweenGroups, world } =
    let
        ( groupA, groupB ) =
            betweenGroups

        listA =
            getMembersOfGroup groupA world

        listB =
            getMembersOfGroup groupB world

        pairs =
            listA
                |> List.concatMap
                    (\a ->
                        List.map
                            (\b ->
                                ( a, b )
                            )
                            listB
                    )

        pairToMaybeMsg :
            ( { objectId : ObjectId
              , group : group
              , object : Physics2d.Object.Object
              }
            , { objectId : ObjectId
              , group : group
              , object : Physics2d.Object.Object
              }
            )
            -> Maybe msg
        pairToMaybeMsg ( a, b ) =
            if Physics2d.Object.areColliding a.object b.object then
                Just (msg { objectA = a, objectB = b })

            else
                Nothing

        msgToCmd : msg -> Cmd msg
        msgToCmd msgToConvert =
            Task.succeed msgToConvert
                |> Task.perform identity
    in
    pairs
        |> List.filterMap pairToMaybeMsg
        |> List.map msgToCmd
        |> Cmd.batch


getMembersOfGroup :
    group
    -> World group
    ->
        List
            { objectId : ObjectId
            , group : group
            , object : Physics2d.Object.Object
            }
getMembersOfGroup groupToMatch (World internals) =
    internals.objects
        |> Dict.map
            (\id ( group, object ) ->
                if group == groupToMatch then
                    Just
                        { objectId = id
                        , group = group
                        , object = object
                        }

                else
                    Nothing
            )
        |> Dict.values
        |> List.filterMap identity


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

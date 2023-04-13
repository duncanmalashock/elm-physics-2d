module Physics2d.World exposing
    ( World
    , init
    , addObject, removeObject
    , getObject
    , update
    , objectViews
    )

{-|

@docs World
@docs init
@docs addObject, removeObject
@docs getObject
@docs update
@docs objectViews

-}

import AssocList as Dict exposing (Dict)
import Length
import List.Extra
import Physics2d.Object
import Random
import Task


type World objectId
    = World (Internals objectId)


type alias Internals objectId =
    { dimensions :
        { x : Length.Length
        , y : Length.Length
        }
    , objects : Dict objectId Physics2d.Object.Object
    }


init :
    { width : Length.Length
    , height : Length.Length
    , objects : List ( objectId, Physics2d.Object.Object )
    }
    -> World objectId
init { width, height, objects } =
    World
        { dimensions =
            { x = width
            , y = height
            }
        , objects = Dict.fromList objects
        }


addObject :
    { id : objectId
    , object : Physics2d.Object.Object
    }
    -> World objectId
    -> World objectId
addObject { id, object } (World internals) =
    World
        { internals
            | objects = Dict.insert id object internals.objects
        }


removeObject : objectId -> World objectId -> World objectId
removeObject objectId (World internals) =
    World
        { internals
            | objects = Dict.remove objectId internals.objects
        }


getObject :
    { id : objectId
    }
    -> World objectId
    -> Maybe Physics2d.Object.Object
getObject { id } (World internals) =
    Dict.get id internals.objects


update :
    { objectUpdateRules :
        List
            (objectId
             -> World objectId
             -> Physics2d.Object.Object
             -> Physics2d.Object.Object
            )
    , objectCreateRules :
        List
            (World objectId
             -> Maybe (Int -> msg)
            )
    , collisionRules :
        List
            ( objectId -> objectId -> Bool
            , ( objectId, Physics2d.Object.Object )
              -> ( objectId, Physics2d.Object.Object )
              -> msg
            )
    , world : World objectId
    }
    -> ( World objectId, Cmd msg )
update { objectUpdateRules, objectCreateRules, collisionRules, world } =
    let
        ( updatedWorld, cmds ) =
            ( world, Cmd.none )
                |> applyObjectUpdateRules objectUpdateRules
                |> integrateObjects
                |> applyObjectCreateRules objectCreateRules
                |> applyCollisionRules collisionRules
    in
    ( updatedWorld, cmds )


applyObjectUpdateRules :
    List
        (objectId
         -> World objectId
         -> Physics2d.Object.Object
         -> Physics2d.Object.Object
        )
    -> ( World objectId, Cmd msg )
    -> ( World objectId, Cmd msg )
applyObjectUpdateRules rules ( (World internals) as world, cmd ) =
    ( World
        { internals
            | objects =
                List.foldl (applyObjectUpdateRule world) internals.objects rules
        }
    , cmd
    )


applyObjectUpdateRule :
    World objectId
    ->
        (objectId
         -> World objectId
         -> Physics2d.Object.Object
         -> Physics2d.Object.Object
        )
    -> Dict objectId Physics2d.Object.Object
    -> Dict objectId Physics2d.Object.Object
applyObjectUpdateRule world rule objects =
    objects
        |> Dict.map (\id -> rule id world)


applyObjectCreateRules :
    List
        (World objectId
         -> Maybe (Int -> msg)
        )
    -> ( World objectId, Cmd msg )
    -> ( World objectId, Cmd msg )
applyObjectCreateRules rules ( (World internals) as world, cmd ) =
    ( World internals
    , Cmd.batch
        (cmd :: List.map (applyObjectCreateRule world) rules)
    )


applyObjectCreateRule :
    World objectId
    ->
        (World objectId
         -> Maybe (Int -> msg)
        )
    -> Cmd msg
applyObjectCreateRule ((World internals) as world) toMaybeIntToMsg =
    case toMaybeIntToMsg world of
        Just toMsg ->
            Random.generate toMsg (Random.int 1 32767)

        Nothing ->
            Cmd.none


applyCollisionRules :
    List
        ( objectId -> objectId -> Bool
        , ( objectId, Physics2d.Object.Object )
          -> ( objectId, Physics2d.Object.Object )
          -> msg
        )
    -> ( World objectId, Cmd msg )
    -> ( World objectId, Cmd msg )
applyCollisionRules rules ( World internals, cmd ) =
    let
        objectPairs =
            Dict.toList internals.objects
                |> List.Extra.uniquePairs
    in
    ( World internals
    , Cmd.batch
        (cmd :: List.concatMap (applyCollisionRule objectPairs) rules)
    )


applyCollisionRule :
    List
        ( ( objectId, Physics2d.Object.Object )
        , ( objectId, Physics2d.Object.Object )
        )
    ->
        ( objectId -> objectId -> Bool
        , ( objectId, Physics2d.Object.Object )
          -> ( objectId, Physics2d.Object.Object )
          -> msg
        )
    -> List (Cmd msg)
applyCollisionRule objectPairs ( predicate, toMsg ) =
    objectPairs
        |> List.filter
            (\( ( id1, object1 ), ( id2, object2 ) ) ->
                predicate id1 id2
            )
        |> List.filterMap
            (\( ( id1, object1 ), ( id2, object2 ) ) ->
                if Physics2d.Object.areColliding object1 object2 then
                    Just (toMsg ( id1, object1 ) ( id2, object2 ))

                else
                    Nothing
            )
        |> List.map
            (\msg ->
                Task.succeed msg
                    |> Task.perform identity
            )


integrateObjects : ( World objectId, Cmd msg ) -> ( World objectId, Cmd msg )
integrateObjects ( World internals, cmd ) =
    ( World
        { internals
            | objects =
                internals.objects
                    |> Dict.map (\id -> Physics2d.Object.integrate)
                    |> Dict.filter
                        (\id object ->
                            Physics2d.Object.shouldRemove object |> not
                        )
        }
    , cmd
    )


objectViews : World objectId -> List Physics2d.Object.View
objectViews (World internals) =
    internals.objects
        |> Dict.values
        |> List.map Physics2d.Object.view

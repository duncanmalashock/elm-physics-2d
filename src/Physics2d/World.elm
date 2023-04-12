module Physics2d.World exposing
    ( World
    , init
    , addObject, removeObject
    , update
    , objectViews
    )

{-|

@docs World
@docs init
@docs addObject, removeObject
@docs update
@docs objectViews

-}

import Angle
import AssocList as Dict exposing (Dict)
import Length
import LineSegment2d
import List.Extra
import Physics2d.CoordinateSystem exposing (TopLeft)
import Physics2d.Object
import Physics2d.Polygon
import Point2d
import Quantity
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
    }
    -> World objectId
init { width, height } =
    World
        { dimensions =
            { x = width
            , y = height
            }
        , objects = Dict.empty
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


update :
    { rules :
        List
            (objectId
             -> World objectId
             -> Physics2d.Object.Object
             -> Physics2d.Object.Object
            )
    , collisionHandlers :
        List
            ( objectId -> objectId -> Bool
            , ( objectId, Physics2d.Object.Object )
              -> ( objectId, Physics2d.Object.Object )
              -> msg
            )
    , world : World objectId
    }
    -> ( World objectId, Cmd msg )
update { rules, world, collisionHandlers } =
    let
        updatedWorld =
            world
                |> applyRules rules
                |> integrateObjects

        cmds =
            applyCollisionHandlers collisionHandlers updatedWorld
    in
    ( updatedWorld, cmds )


applyRules :
    List
        (objectId
         -> World objectId
         -> Physics2d.Object.Object
         -> Physics2d.Object.Object
        )
    -> World objectId
    -> World objectId
applyRules rules (World internals) =
    World
        { internals
            | objects =
                List.foldl (applyRule (World internals)) internals.objects rules
        }


applyRule :
    World objectId
    ->
        (objectId
         -> World objectId
         -> Physics2d.Object.Object
         -> Physics2d.Object.Object
        )
    -> Dict objectId Physics2d.Object.Object
    -> Dict objectId Physics2d.Object.Object
applyRule world rule objects =
    objects
        |> Dict.map (\id -> rule id world)


applyCollisionHandlers :
    List
        ( objectId -> objectId -> Bool
        , ( objectId, Physics2d.Object.Object )
          -> ( objectId, Physics2d.Object.Object )
          -> msg
        )
    -> World objectId
    -> Cmd msg
applyCollisionHandlers collisionHandlers (World internals) =
    let
        objectPairs =
            Dict.toList internals.objects
                |> List.Extra.uniquePairs
    in
    collisionHandlers
        |> List.concatMap (applyCollisionHandler objectPairs)
        |> Cmd.batch


applyCollisionHandler :
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
applyCollisionHandler objectPairs ( predicate, toMsg ) =
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


integrateObjects : World objectId -> World objectId
integrateObjects (World internals) =
    World
        { internals
            | objects =
                internals.objects
                    |> Dict.map (\id -> Physics2d.Object.integrate)
        }


objectViews : World objectId -> List Physics2d.Object.View
objectViews (World internals) =
    internals.objects
        |> Dict.values
        |> List.map Physics2d.Object.view

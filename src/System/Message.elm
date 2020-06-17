module System.Message exposing
    ( SystemMessage
    , spawn, spawnWithFlags, populateView, populateAddress, spawnMultiple, spawnMultipleWithFlags
    , kill, removeFromView, removeFromAddress
    , sendToPid, sendToAddress, sendToPidOnAddress
    , batch, noOperation, toCmd
    , updateDocumentTitle
    , log, ignoreLog
    )

{-|


# System Messages

@docs SystemMessage


# Spawning and Populating

Spawn an Actor and add it to the System's view

    spawn SomeActor populateView

Spawn an Actor, add it to the System's view, and assign it an Address

    spawn
        SomeActor
        (\pid ->
            batch
                [ populateView pid
                , populateAddress pid
                ]
        )

@docs spawn, spawnWithFlags, populateView, populateAddress, spawnMultiple, spawnMultipleWithFlags


# Removing and Destroying

@docs kill, removeFromView, removeFromAddress


# Actor Communication

@docs sendToPid, sendToAddress, sendToPidOnAddress


# Utility

@docs batch, noOperation, toCmd


# Application

@docs updateDocumentTitle


# Log

@docs log, ignoreLog

-}

import Json.Encode as Encode exposing (Value)
import System.Internal.Message as Internal exposing (Control(..), LogMessage, SystemMessage(..))
import System.Internal.PID exposing (PID)
import Task as Task


{-| The type of the System Messages
-}
type alias SystemMessage addresses actors appMsg =
    Internal.SystemMessage addresses actors appMsg



-- Spawning and Populating


{-| Spawn an Actor
-}
spawn :
    actors
    -> (PID -> SystemMessage addresses actors appMsg)
    -> SystemMessage addresses actors appMsg
spawn actors =
    Control << SpawnWithFlags Encode.null actors


{-| Spawn an Actor with given flags (as an encoded JSON Value)
-}
spawnWithFlags :
    Value
    -> actors
    -> (PID -> SystemMessage addresses actors appMsg)
    -> SystemMessage addresses actors appMsg
spawnWithFlags flags actors =
    Control << SpawnWithFlags flags actors


{-| Spawn multiple Actors
-}
spawnMultiple :
    List actors
    -> (List PID -> SystemMessage addresses actors appMsg)
    -> SystemMessage addresses actors appMsg
spawnMultiple listactors =
    Control << SpawnMultipleWithFlags (List.map (\a -> ( a, Encode.null )) listactors)


{-| Spawn multiple Actors with given flags
-}
spawnMultipleWithFlags :
    List ( actors, Value )
    -> (List PID -> SystemMessage addresses actors appMsg)
    -> SystemMessage addresses actors appMsg
spawnMultipleWithFlags listActorNamesAndFlags =
    Control << SpawnMultipleWithFlags listActorNamesAndFlags


{-| Add a PID to the System's view

The System will render views in the order it receives it.

-}
populateView :
    PID
    -> SystemMessage addresses actors appMsg
populateView =
    Control << AddView


{-| Add a PID to a given addresses

You can send messages to Addresses just like you can send messages to a PID.

-}
populateAddress :
    addresses
    -> PID
    -> SystemMessage addresses actors appMsg
populateAddress addresses =
    Control << PopulateAddress addresses



-- Destroying


{-| Kill a process

This will trigger the Actors onKill event on which you can decide what to do with this Message.

There is a Default behaviour available that will remove the Process from the System.

-}
kill :
    PID
    -> SystemMessage addresses actors appMsg
kill =
    Control << Kill


{-| Remove a PID from a given addresses
-}
removeFromAddress :
    addresses
    -> PID
    -> SystemMessage addresses actors appMsg
removeFromAddress addresses =
    Control << RemoveFromAddress addresses


{-| Remove a PID from the System view
-}
removeFromView :
    PID
    -> SystemMessage addresses actors appMsg
removeFromView =
    Control << RemoveFromView



-- Actor Communication


{-| Send a message to a PID.
-}
sendToPid :
    PID
    -> appMsg
    -> SystemMessage addresses actors appMsg
sendToPid pid =
    ActorMsg
        >> SendToPID pid
        >> Control


{-| Send a message to an _addresses_.
-}
sendToAddress :
    addresses
    -> appMsg
    -> SystemMessage addresses actors appMsg
sendToAddress addresses =
    ActorMsg
        >> SendToAddress addresses
        >> Control


{-| Send a message to a PID **only** when it's on the given _addresses_.
-}
sendToPidOnAddress :
    PID
    -> addresses
    -> appMsg
    -> SystemMessage addresses actors appMsg
sendToPidOnAddress pid addresses =
    ActorMsg
        >> SendToPidOnAddress pid addresses
        >> Control



-- Utility


{-| Batch perform a list of messages

    spawn MyActorWorker
        (\pid ->
            batch
                [ populateAddress pid
                , populateView pid
                ]
        )

-}
batch :
    List (SystemMessage addresses actors appMsg)
    -> SystemMessage addresses actors appMsg
batch =
    Control << Batch


{-| Don't do anything

    spaw MyActorWorker (always noOperation)

-}
noOperation : SystemMessage addresses actors appMsg
noOperation =
    NoOp


{-| Converts a generic msg into Cmd.
-}
toCmd :
    msg
    -> Cmd msg
toCmd =
    Task.perform identity << Task.succeed



-- Application


{-| Update the document title
-}
updateDocumentTitle :
    String
    -> SystemMessage addresses actors appMsg
updateDocumentTitle =
    UpdateDocumentTitle



-- Log


{-| Convenience function that ignores all logs
-}
ignoreLog :
    LogMessage addresses actors appMsg
    -> SystemMessage addresses actors appMsg
ignoreLog =
    always noOperation


{-| Log a LogMessage

This will trigger the onLogMessage function you provided while initializing your application.

-}
log :
    LogMessage addresses actors appMsg
    -> SystemMessage addresses actors appMsg
log =
    Log

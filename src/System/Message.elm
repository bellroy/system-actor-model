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
type alias SystemMessage applicationAddress applicationActorName applicationMessage =
    Internal.SystemMessage applicationAddress applicationActorName applicationMessage



-- Spawning and Populating


{-| Spawn an Actor
-}
spawn :
    applicationActorName
    -> (PID -> SystemMessage applicationAddress applicationActorName applicationMessage)
    -> SystemMessage applicationAddress applicationActorName applicationMessage
spawn applicationActorName =
    Control << SpawnWithFlags Encode.null applicationActorName


{-| Spawn an Actor with given flags (as an encoded JSON Value)
-}
spawnWithFlags :
    Value
    -> applicationActorName
    -> (PID -> SystemMessage applicationAddress applicationActorName applicationMessage)
    -> SystemMessage applicationAddress applicationActorName applicationMessage
spawnWithFlags flags applicationActorName =
    Control << SpawnWithFlags flags applicationActorName


{-| Spawn multiple Actors
-}
spawnMultiple :
    List applicationActorName
    -> (List PID -> SystemMessage applicationAddress applicationActorName applicationMessage)
    -> SystemMessage applicationAddress applicationActorName applicationMessage
spawnMultiple listApplicationActorName =
    Control << SpawnMultipleWithFlags (List.map (\a -> ( a, Encode.null )) listApplicationActorName)


{-| Spawn multiple Actors with given flags
-}
spawnMultipleWithFlags :
    List ( applicationActorName, Value )
    -> (List PID -> SystemMessage applicationAddress applicationActorName applicationMessage)
    -> SystemMessage applicationAddress applicationActorName applicationMessage
spawnMultipleWithFlags listActorNamesAndFlags =
    Control << SpawnMultipleWithFlags listActorNamesAndFlags


{-| Add a PID to the System's view

The System will render views in the order it receives it.

-}
populateView :
    PID
    -> SystemMessage applicationAddress applicationActorName applicationMessage
populateView =
    Control << AddView


{-| Add a PID to a given applicationAddress

You can send messages to Addresses just like you can send messages to a PID.

-}
populateAddress :
    applicationAddress
    -> PID
    -> SystemMessage applicationAddress applicationActorName applicationMessage
populateAddress applicationAddress =
    Control << PopulateAddress applicationAddress



-- Destroying


{-| Kill a process

This will trigger the Actors onKill event on which you can decide what to do with this Message.

There is a Default behaviour available that will remove the Process from the System.

-}
kill :
    PID
    -> SystemMessage applicationAddress applicationActorName applicationMessage
kill =
    Control << Kill


{-| Remove a PID from a given applicationAddress
-}
removeFromAddress :
    applicationAddress
    -> PID
    -> SystemMessage applicationAddress applicationActorName applicationMessage
removeFromAddress applicationAddress =
    Control << RemoveFromAddress applicationAddress


{-| Remove a PID from the System view
-}
removeFromView :
    PID
    -> SystemMessage applicationAddress applicationActorName applicationMessage
removeFromView =
    Control << RemoveFromView



-- Actor Communication


{-| Send a message to a PID.
-}
sendToPid :
    PID
    -> applicationMessage
    -> SystemMessage applicationAddress applicationActorName applicationMessage
sendToPid pid =
    ActorMsg
        >> SendToPID pid
        >> Control


{-| Send a message to an _applicationAddress_.
-}
sendToAddress :
    applicationAddress
    -> applicationMessage
    -> SystemMessage applicationAddress applicationActorName applicationMessage
sendToAddress applicationAddress =
    ActorMsg
        >> SendToAddress applicationAddress
        >> Control


{-| Send a message to a PID **only** when it's on the given _applicationAddress_.
-}
sendToPidOnAddress :
    PID
    -> applicationAddress
    -> applicationMessage
    -> SystemMessage applicationAddress applicationActorName applicationMessage
sendToPidOnAddress pid applicationAddress =
    ActorMsg
        >> SendToPidOnAddress pid applicationAddress
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
    List (SystemMessage applicationAddress applicationActorName applicationMessage)
    -> SystemMessage applicationAddress applicationActorName applicationMessage
batch =
    Control << Batch


{-| Don't do anything

    spaw MyActorWorker (always noOperation)

-}
noOperation : SystemMessage applicationAddress applicationActorName applicationMessage
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
    -> SystemMessage applicationAddress applicationActorName applicationMessage
updateDocumentTitle =
    UpdateDocumentTitle



-- Log


{-| Convenience function that ignores all logs
-}
ignoreLog :
    LogMessage applicationAddress applicationActorName applicationMessage
    -> SystemMessage applicationAddress applicationActorName applicationMessage
ignoreLog =
    always noOperation


{-| Log a LogMessage

This will trigger the onLogMessage function you provided while initializing your application.

-}
log :
    LogMessage applicationAddress applicationActorName applicationMessage
    -> SystemMessage applicationAddress applicationActorName applicationMessage
log =
    Log

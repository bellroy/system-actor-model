module System.Message exposing
    ( SystemMessage
    , spawn, spawnWithFlags, populateView, populateAddress
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

@docs spawn, spawnWithFlags, populateView, populateAddress


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

import Json.Encode as Encode
import System.Internal.Message exposing (Control(..), LogMessage, Message(..))
import System.Internal.PID exposing (PID)
import Task as Task exposing (perform, succeed)


{-| The type of the System Messages
-}
type alias SystemMessage address actorName appMsg =
    Message address actorName appMsg



-- Spawning and Populating


{-| Spawn an Actor
-}
spawn :
    actorName
    -> (PID -> Message address actorName appMsg)
    -> Message address actorName appMsg
spawn actorName =
    Control << Spawn actorName


{-| Spawn an Actor with given flags (as an encoded JSON Value)
-}
spawnWithFlags :
    Encode.Value
    -> actorName
    -> (PID -> Message address actorName appMsg)
    -> Message address actorName appMsg
spawnWithFlags flags actorName =
    Control << SpawnWithFlags flags actorName


{-| Add a PID to the System's view

The System will render views in the order it receives it.

-}
populateView :
    PID
    -> Message address actorName appMsg
populateView =
    Control << AddView


{-| Add a PID to a given address

You can send messages to Addresses just like you can send messages to a PID.

-}
populateAddress :
    address
    -> PID
    -> Message address actorName appMsg
populateAddress address =
    Control << PopulateAddress address



-- Destroying


{-| Kill a process

This will trigger the Actors onKill event on which you can decide what to do with this Message.

There is a Default behaviour available that will remove the Process from the System.

-}
kill :
    PID
    -> Message address actorName appMsg
kill =
    Control << Kill


{-| Remove a PID from a given address
-}
removeFromAddress :
    address
    -> PID
    -> Message address actorName appMsg
removeFromAddress address =
    Control << RemoveFromAddress address


{-| Remove a PID from the System view
-}
removeFromView :
    PID
    -> Message address actorName appMsg
removeFromView =
    Control << RemoveFromView



-- Actor Communication


{-| Send a message to a PID.
-}
sendToPid :
    PID
    -> appMsg
    -> Message address actorName appMsg
sendToPid pid =
    ActorMsg
        >> SendToPID pid
        >> Control


{-| Send a message to an _address_.
-}
sendToAddress :
    address
    -> appMsg
    -> Message address actorName appMsg
sendToAddress address =
    ActorMsg
        >> SendToAddress address
        >> Control


{-| Send a message to a PID **only** when it's on the given _address_.
-}
sendToPidOnAddress :
    PID
    -> address
    -> appMsg
    -> Message address actorName appMsg
sendToPidOnAddress pid address =
    ActorMsg
        >> SendToPidOnAddress pid address
        >> Control



-- Utility


{-| Batch perform a list of messages
-}
batch :
    List (Message address actorName appMsg)
    -> Message address actorName appMsg
batch =
    Control << Batch


{-| Don't do anythinge

This can be handy for instance when you want to spawn an Actor but don't do anything with it's resulting PID.

    spaw MyActorWorker (always noOperation)

-}
noOperation : Message address actorName appMsg
noOperation =
    NoOp


{-| Converts a msg in to Cmd.
-}
toCmd :
    msg
    -> Cmd msg
toCmd msg =
    Task.perform identity (Task.succeed msg)



-- Application


{-| Update the document title
-}
updateDocumentTitle :
    String
    -> Message address actorName appMsg
updateDocumentTitle =
    UpdateDocumentTitle



-- Log


{-| convenience function that ignores all logs
-}
ignoreLog :
    LogMessage address actorName appMsg
    -> Message address actorName appMsg
ignoreLog =
    always noOperation


{-| Log a LogMessage

This will trigger the onLogMessage function you provided while initializing your application.

-}
log :
    LogMessage address actorName appMsg
    -> Message address actorName appMsg
log =
    Log

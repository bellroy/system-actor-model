module System.Message exposing
    ( SystemMessage
    , spawn, populateView, populateAddress
    , kill
    , sendToPid, sendToAddress
    , batch, noOperation, toCmd
    , updateDocumentTitle
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

@docs spawn, populateView, populateAddress


# Destroying

@docs kill


# Actor Communication

@docs sendToPid, sendToAddress


# Utility

@docs batch, noOperation, toCmd


# Application

@docs updateDocumentTitle

-}

import System.Internal.Message exposing (Control(..), Message(..))
import System.Internal.PID exposing (PID)
import Task as Task exposing (perform, succeed)


{-| The type of the System Messages
-}
type alias SystemMessage address actorName wrappedMsg =
    Message address actorName wrappedMsg



-- Spawning and Populating


{-| Spawn an Actor
-}
spawn :
    actorName
    -> (PID -> Message address actorName wrappedMsg)
    -> Message address actorName wrappedMsg
spawn actorName f =
    Control <| Spawn actorName f


{-| Add a PID to the System's view

The System will render views in the order it receives it.

-}
populateView :
    PID
    -> Message address actorName wrappedMsg
populateView =
    Control << AddView


{-| Add a PID to the System's view

The System will render views in the order it receives it.

-}
populateAddress :
    address
    -> PID
    -> Message address actorName wrappedMsg
populateAddress address =
    Control << PopulateAddress address



-- Destroying


{-| Kill a process

This will trigger the Actors onKill event on which you can decide what to do with this Message.

There is a Default behaviour available that will remove the Process from the System.

-}
kill :
    PID
    -> Message address actorName wrappedMsg
kill =
    Control << Kill



-- Actor Communication


{-| Send a message to a PID.
-}
sendToPid :
    PID
    -> wrappedMsg
    -> Message address actorName wrappedMsg
sendToPid pid =
    ActorMsg
        >> SendToPID pid
        >> Control


{-| Send a message to an Address
-}
sendToAddress :
    address
    -> wrappedMsg
    -> Message address actorName wrappedMsg
sendToAddress address wrappedMsg =
    Control <| SendToAddress address <| ActorMsg wrappedMsg



-- Utility


{-| Batch perform a list of messages
-}
batch :
    List (Message address actorName wrappedMsg)
    -> Message address actorName wrappedMsg
batch =
    Control << Batch


{-| Don't do anythinge

This can be handy for instance when you want to spawn an Actor but don't do anything with it's resulting PID.

    spaw MyActorWorker (always noOperation)

-}
noOperation : Message address actorName wrappedMsg
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
    -> Message address actorName wrappedMsg
updateDocumentTitle =
    UpdateDocumentTitle

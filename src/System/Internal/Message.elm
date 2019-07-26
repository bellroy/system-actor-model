module System.Internal.Message exposing (Message(..), Control(..))

{-|


# Message

@docs Message, Control

-}

import System.Internal.PID exposing (PID)


{-| The System Message Type
-}
type Message address actorName wrappedMsg
    = NoOp
    | ActorMsg wrappedMsg
    | UnmappedMsg wrappedMsg
    | Control (Control address actorName (Message address actorName wrappedMsg))
    | Context PID (Message address actorName wrappedMsg)
    | UpdateDocumentTitle String


{-| The Ctrl Messages
-}
type Control address actorName message
    = Batch (List message)
    | Command (Cmd message)
    | SendToPID PID message
    | SendToAddress address message
    | Spawn actorName (PID -> message)
    | AddView PID
    | PopulateAddress address PID
    | Kill PID

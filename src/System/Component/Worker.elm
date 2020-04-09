module System.Component.Worker exposing
    ( Worker
    , toActor
    )

{-|


# Worker

A Worker is a [headless](https://en.wikipedia.org/wiki/Headless_software) Actor, it has no User Interface.

This is great if you want to use an Actor as the “brain” for something else.


## Example usage

    type alias Model =
        String

    type MsgIn
        = OnStuff

    type MsgOut
        = DoStuff

    component : Worker Model MsgIn MsgOut
    component =
        { init =
            \_ ->
                ( "Worker Model"
                , []
                , Cmd.none
                )
        , update =
            \msgIn model ->
                case msgIn of
                    onStuff ->
                        ( model
                        , [ DoStuff ]
                        , Cmd.none
                        )
        , subscriptions = always Sub.none
        , events = System.Event.ignoreAll
        }


## Types

@docs Worker


## Creation

@docs toActor

-}

import Json.Decode exposing (Value)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component as Component
import System.Internal.Message exposing (SystemMessage)
import System.Process exposing (PID)


{-| The Type of a Worker Component
-}
type alias Worker componentModel componentMsgIn componentMsgOut =
    { init :
        ( PID, Value )
        -> ( componentModel, List componentMsgOut, Cmd componentMsgIn )
    , update :
        componentMsgIn
        -> componentModel
        -> ( componentModel, List componentMsgOut, Cmd componentMsgIn )
    , subscriptions :
        componentModel
        -> Sub componentMsgIn
    , events : ComponentEventHandlers componentMsgIn
    }


{-| Create an Actor from a Worker
-}
toActor :
    Worker componentModel componentMsgIn componentMsgOut
    ->
        { wrapModel : componentModel -> model
        , wrapMsg : componentMsgIn -> applicationMessage
        , mapIn : applicationMessage -> Maybe componentMsgIn
        , mapOut :
            PID
            -> componentMsgOut
            -> SystemMessage address actorName applicationMessage
        }
    -> Actor componentModel model componentOutput (SystemMessage address actorName applicationMessage)
toActor worker args =
    { init = Component.wrapInit args worker.init
    , update = Component.wrapUpdate args worker.update
    , subscriptions = Component.wrapSubscriptions args worker.subscriptions
    , events = Component.wrapEvents args worker.events
    , view = Nothing
    }

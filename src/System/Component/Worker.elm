module System.Component.Worker exposing (Worker, toActor)

{-|


# Worker

A Worker is a [headless](https://en.wikipedia.org/wiki/Headless_software) Actor, it has no user interface.

This is great if you want to use an Actor as the “brain” for something else.

@docs Worker, toActor

-}

import Html exposing (Html, text)
import Json.Decode exposing (Value)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component exposing (wrapEvents, wrapInit, wrapSubscriptions, wrapUpdate)
import System.Internal.Message exposing (Message)
import System.Process exposing (PID)


{-| The Type of a Worker Component
-}
type alias Worker model msgIn msgOut =
    { init :
        ( PID, Value )
        -> ( model, List msgOut, Cmd msgIn )
    , update :
        msgIn
        -> model
        -> ( model, List msgOut, Cmd msgIn )
    , subscriptions :
        model
        -> Sub msgIn
    , events : ComponentEventHandlers msgIn
    }


{-| Create an Actor from a Worker
-}
toActor :
    Worker model msgIn msgOut
    ->
        { wrapModel : model -> actorModel
        , wrapMsg : msgIn -> appMsg
        , mapIn : appMsg -> Maybe msgIn
        , mapOut :
            PID
            -> msgOut
            -> Message address actorName appMsg
        }
    -> Actor model actorModel (Html msg) (Message address actorName appMsg)
toActor worker args =
    { init = wrapInit args worker.init
    , update = wrapUpdate args worker.update
    , subscriptions = wrapSubscriptions args worker.subscriptions
    , events = wrapEvents args worker.events

    -- Disable the view by always rendering an empty string
    , view = \_ _ _ -> text ""
    }

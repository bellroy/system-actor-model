module System.Component.Worker exposing (Worker, toActor)

{-|


# Worker

A Worker is a [headless](https://en.wikipedia.org/wiki/Headless_software) Actor, it has no user interface.

This is great if you want to use an Actor as the “brain” for something else.

@docs Worker, toActor

-}

import Html as Html exposing (Html, text)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component exposing (wrapEvents, wrapInit, wrapSubscriptions, wrapToTuple, wrapUpdate)
import System.Internal.Message exposing (Message)
import System.Process exposing (PID)


{-| The Type of a Worker Component
-}
type alias Worker model msgIn msgOut =
    { init :
        PID
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
        , wrapMsg : msgIn -> wrappedMsg
        , mapIn : wrappedMsg -> Maybe msgIn
        , mapOut :
            PID
            -> msgOut
            -> Message address actorName wrappedMsg
        }
    -> Actor model actorModel (Html.Html msg) (Message address actorName wrappedMsg)
toActor worker ({ wrapModel, wrapMsg, mapIn } as args) =
    { init = wrapInit args worker.init
    , update = wrapUpdate args worker.update
    , subscriptions = wrapSubscriptions args worker.subscriptions
    , events = wrapEvents args worker.events

    -- Disable the view by always rendering an empty string
    , view = \_ _ _ -> Html.text ""
    }

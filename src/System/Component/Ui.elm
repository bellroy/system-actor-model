module System.Component.Ui exposing (Ui, toActor)

{-|


# Ui

A Ui component can't spawn or render other Actors.

@docs Ui, toActor

-}

import Html exposing (Html)
import Json.Decode exposing (Value)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component exposing (wrapEvents, wrapInit, wrapSubscriptions, wrapUiView, wrapUpdate)
import System.Internal.Message exposing (Message)
import System.Process exposing (PID)


{-| The Type of a Ui Component
-}
type alias Ui model msgIn msgOut =
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
    , view : model -> Html msgIn
    }


{-| Create an Actor from a Ui
-}
toActor :
    Ui model msgIn msgOut
    ->
        { wrapModel : model -> actorModel
        , wrapMsg : msgIn -> appMsg
        , mapIn : appMsg -> Maybe msgIn
        , mapOut :
            PID
            -> msgOut
            -> Message address actorName appMsg
        }
    -> Actor model actorModel (Html (Message address actorName appMsg)) (Message address actorName appMsg)
toActor ui args =
    { init = wrapInit args ui.init
    , update = wrapUpdate args ui.update
    , subscriptions = wrapSubscriptions args ui.subscriptions
    , events = wrapEvents args ui.events
    , view = wrapUiView args ui.view
    }

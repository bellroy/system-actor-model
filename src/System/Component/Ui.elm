module System.Component.Ui exposing (Ui, toActor)

{-|


# Ui

A Ui component can't spawn or render other Actors.

@docs Ui, toActor

-}

import Html as Html exposing (Html, text)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component exposing (wrapEvents, wrapInit, wrapSubscriptions, wrapToTuple, wrapUiView, wrapUpdate)
import System.Internal.Message exposing (Message)
import System.Process exposing (PID)


{-| The Type of a Ui Component
-}
type alias Ui model msgIn msgOut =
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
    , view : model -> Html.Html msgIn
    }


{-| Create an Actor from a Ui
-}
toActor :
    Ui model msgIn msgOut
    ->
        { wrapModel : model -> actorModel
        , wrapMsg : msgIn -> wrappedMsg
        , mapIn : wrappedMsg -> Maybe msgIn
        , mapOut :
            PID
            -> msgOut
            -> Message address actorName wrappedMsg
        }
    -> Actor model actorModel (Html (Message address actorName wrappedMsg)) (Message address actorName wrappedMsg)
toActor ui ({ wrapModel, wrapMsg, mapIn } as args) =
    { init = wrapInit args ui.init
    , update = wrapUpdate args ui.update
    , subscriptions = wrapSubscriptions args ui.subscriptions
    , events = wrapEvents args ui.events
    , view = wrapUiView args ui.view
    }

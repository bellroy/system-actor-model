module System.Component.Layout exposing (Layout, toActor)

{-|


# Layout

A Layout is a component that can spawn, hold and render other components.

@docs Layout, toActor

-}

import Html as Html exposing (Html, text)
import Json.Decode as Decode
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component exposing (wrapEvents, wrapInit, wrapLayoutView, wrapSubscriptions, wrapToTuple, wrapUpdate)
import System.Internal.Message exposing (Message)
import System.Process exposing (PID)


{-| The Type of a Layout Component
-}
type alias Layout model msgIn msgOut msg =
    { init :
        ( PID, Decode.Value )
        -> ( model, List msgOut, Cmd msgIn )
    , update :
        msgIn
        -> model
        -> ( model, List msgOut, Cmd msgIn )
    , subscriptions :
        model
        -> Sub msgIn
    , events : ComponentEventHandlers msgIn
    , view : (msgIn -> msg) -> model -> (PID -> Html.Html msg) -> Html.Html msg
    }


{-| Create an Actor from a Layout
-}
toActor :
    Layout model msgIn msgOut (Message address actorName wrappedMsg)
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
toActor layout ({ wrapModel, wrapMsg, mapIn } as args) =
    { init = wrapInit args layout.init
    , update = wrapUpdate args layout.update
    , subscriptions = wrapSubscriptions args layout.subscriptions
    , events = wrapEvents args layout.events
    , view = wrapLayoutView args layout.view
    }

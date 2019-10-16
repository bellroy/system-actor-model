module System.Component.Layout exposing (Layout, toActor)

{-|


# Layout

A Layout is a component that can spawn, hold and render other components.

@docs Layout, toActor

-}

import Html exposing (Html)
import Json.Decode exposing (Value)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component exposing (wrapEvents, wrapInit, wrapLayoutView, wrapSubscriptions, wrapUpdate)
import System.Internal.Message exposing (Message)
import System.Process exposing (PID)


{-| The Type of a Layout Component
-}
type alias Layout model msgIn msgOut msg =
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
    , view : (msgIn -> msg) -> model -> (PID -> Html.Html msg) -> Html.Html msg
    }


{-| Create an Actor from a Layout
-}
toActor :
    Layout model msgIn msgOut (Message address actorName appMsg)
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
toActor layout args =
    { init = wrapInit args layout.init
    , update = wrapUpdate args layout.update
    , subscriptions = wrapSubscriptions args layout.subscriptions
    , events = wrapEvents args layout.events
    , view = wrapLayoutView args layout.view
    }

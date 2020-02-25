module System.Component.Ui exposing
    ( Ui
    , toActor
    )

{-|


# Ui

A Ui component **can't** render other Actors.


## Example usage

    type alias Model =
        Int

    type MsgIn
        = Increment
        | Decrement

    type MsgOut
        = NoMsgOut

    component : Ui Model MsgIn MsgOut
    component =
        { init =
            \_ ->
                ( 0, [], Cmd.none )
        , update =
            \msgIn model ->
                case msgIn of
                    Increment ->
                        ( model + 1, [], Cmd.none )

                    Decrement ->
                        ( model - 1, [], Cmd.none )
        , view =
            \model ->
                Html.div []
                    [ Html.button
                        [ Html.Events.onClick Decrement
                        ]
                        [ Html.text "-"
                        ]
                    , Html.text <| String.fromInt model
                    , Html.button
                        [ Html.Events.onClick Increment
                        ]
                        [ Html.text "+"
                        ]
                    ]
        , subscriptions = always Sub.none
        , events = System.Event.ignoreAll
        }


## Types

@docs Ui


## Creation

@docs toActor

-}

import Html exposing (Html)
import Json.Decode exposing (Value)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component as Component
import System.Internal.Message exposing (SystemMessage)
import System.Process exposing (PID)


{-| The Type of an Ui Component
-}
type alias Ui componentModel componentMsgIn componentMsgOut =
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
    , view : componentModel -> Html componentMsgIn
    }


{-| Create an Actor from an Ui Component
-}
toActor :
    Ui componentModel componentMsgIn componentMsgOut
    ->
        { wrapModel : componentModel -> applicationModel
        , wrapMsg : componentMsgIn -> applicationMessage
        , mapIn : applicationMessage -> Maybe componentMsgIn
        , mapOut :
            PID
            -> componentMsgOut
            -> SystemMessage address componentName applicationMessage
        }
    -> Actor componentModel applicationModel (Html (SystemMessage address componentName applicationMessage)) (SystemMessage address componentName applicationMessage)
toActor ui args =
    { init = Component.wrapInit args ui.init
    , update = Component.wrapUpdate args ui.update
    , subscriptions = Component.wrapSubscriptions args ui.subscriptions
    , events = Component.wrapEvents args ui.events
    , view = Component.wrapUiView args ui.view
    }

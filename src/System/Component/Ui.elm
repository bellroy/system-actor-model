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
            \toSelf model ->
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

import Json.Decode exposing (Value)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component as Component
import System.Internal.Message exposing (SystemMessage)
import System.Process exposing (PID)


{-| The Type of an Ui Component
-}
type alias Ui componentOutput componentModel componentMsgIn componentMsgOut msg =
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
    , view : (componentMsgIn -> msg) -> componentModel -> componentOutput
    }


{-| Create an Actor from an Ui Component
-}
toActor :
    Ui componentOutput componentModel componentMsgIn componentMsgOut (SystemMessage address actorName appMsg)
    ->
        { wrapModel : componentModel -> applicationModel
        , wrapMsg : componentMsgIn -> appMsg
        , mapIn : appMsg -> Maybe componentMsgIn
        , mapOut :
            PID
            -> componentMsgOut
            -> SystemMessage address actorName appMsg
        }
    -> Actor componentModel applicationModel componentOutput (SystemMessage address actorName appMsg)
toActor ui args =
    { init = Component.wrapInit args ui.init
    , update = Component.wrapUpdate args ui.update
    , subscriptions = Component.wrapSubscriptions args ui.subscriptions
    , events = Component.wrapEvents args ui.events
    , view = Just <| Component.wrapUiView args ui.view
    }

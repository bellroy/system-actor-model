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

    component : Ui (Html msg) Model MsgIn MsgOut msg
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
                        [ Html.Events.onClick <|
                            toSelf Decrement
                        ]
                        [ Html.text "-"
                        ]
                    , Html.text <| String.fromInt model
                    , Html.button
                        [ Html.Events.onClick <|
                            toSelf Increment
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
type alias Ui output componentModel componentMsgIn componentMsgOut msg =
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
    , view : (componentMsgIn -> msg) -> componentModel -> output
    }


{-| Create an Actor from an Ui Component
-}
toActor :
    Ui output componentModel componentMsgIn componentMsgOut (SystemMessage address actorName appMsg)
    ->
        { wrapModel : componentModel -> appModel
        , wrapMsg : componentMsgIn -> appMsg
        , mapIn : appMsg -> Maybe componentMsgIn
        , mapOut :
            PID
            -> componentMsgOut
            -> SystemMessage address actorName appMsg
        }
    -> Actor componentModel appModel output (SystemMessage address actorName appMsg)
toActor ui args =
    { init = Component.wrapInit args ui.init
    , update = Component.wrapUpdate args ui.update
    , subscriptions = Component.wrapSubscriptions args ui.subscriptions
    , events = Component.wrapEvents args ui.events
    , view = Just <| Component.wrapUiView args ui.view
    }

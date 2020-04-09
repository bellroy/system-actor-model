module System.Component.Layout exposing
    ( Layout
    , toActor
    )

{-|


# Layout

A Layout is a component that can render other components.


## Example usage

    type alias Model =
        { instances : List System.Process.PID
        }

    type MsgIn
        = AddProcess System.Process.PID
        | OnClickKillProcess System.Process.PID
        | ProcessKilled System.Process.PID

    type MsgOut
        = KillProcess System.Process.PID

    component : Layout Model MsgIn MsgOut layoutChildMsgs
    component =
        { init =
            \_ ->
                ( { instances = [] }
                , []
                , Cmd.none
                )
        , update =
            \msgIn model ->
                case msgIn of
                    AddProcess pid ->
                        ( { model
                            | instances = pid :: model.instances
                          }
                        , []
                        , Cmd.none
                        )

                    OnClickKillProcess pid ->
                        ( model
                        , [ KillProcess pid ]
                        , Cmd.none
                        )

                    ProcessKilled pid ->
                        ( { model
                            | instances = List.filter (not << System.Process.equals pid)
                          }
                        , []
                        , Cmd.none
                        )
        , view =
            \toSelf model renderPid ->
                model.instances
                    |> List.map
                        (\pid ->
                            Html.div []
                                [ Html.button [ Html.Events.onClick (OnClickKillProcess pid) ] [ Html.text "kill process" ]
                                    |> Html.map toSelf
                                , renderPid pid
                                ]
                        )
                    |> Html.div []
        , subscriptions = always Sub.none
        , events = System.Event.ignoreAll
        }


## Types

@docs Layout


## Creation

@docs toActor

-}

import Json.Decode exposing (Value)
import System.Actor exposing (Actor)
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Component as Component
import System.Internal.Message exposing (SystemMessage)
import System.Process exposing (PID)


{-| The Type of a Layout Component
-}
type alias Layout componentOutput componentModel componentMsgIn componentMsgOut msg =
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
    , view :
        (componentMsgIn -> msg)
        -> componentModel
        -> (PID -> Maybe componentOutput)
        -> componentOutput
    }


{-| Create an Actor from a Layout Component
-}
toActor :
    Layout componentOutput componentModel componentMsgIn componentMsgOut (SystemMessage address actorName appMsg)
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
toActor layout args =
    { init = Component.wrapInit args layout.init
    , update = Component.wrapUpdate args layout.update
    , subscriptions = Component.wrapSubscriptions args layout.subscriptions
    , events = Component.wrapEvents args layout.events
    , view = Just <| Component.wrapLayoutView args layout.view
    }

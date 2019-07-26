module Components.Counters exposing (Model, MsgIn(..), MsgOut(..), component)

import Html exposing (Html, button, div, h1, option, p, select, table, text, th, tr)
import Html.Events exposing (onClick)
import System.Component.Layout exposing (Layout)
import System.Debug exposing (pidToString)
import System.Event exposing (ignoreAll)
import System.Process exposing (PID, equals)


type Model
    = Counters PID (List PID)


type MsgIn
    = ReceiveCounter PID
    | OnSpawnCounterClick
    | OnKillCounterClick
    | KilledCounter PID


type MsgOut
    = SpawnCounter
    | KillCounter PID


component : Layout Model MsgIn MsgOut msg
component =
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    , events = ignoreAll
    }


init :
    PID
    -> ( Model, List MsgOut, Cmd MsgIn )
init pid =
    ( Counters pid [], [], Cmd.none )


update :
    MsgIn
    -> Model
    -> ( Model, List MsgOut, Cmd MsgIn )
update msgIn ((Counters pid counters) as model) =
    case msgIn of
        ReceiveCounter counterPid ->
            ( Counters pid (counterPid :: counters), [], Cmd.none )

        OnSpawnCounterClick ->
            ( model, [ SpawnCounter ], Cmd.none )

        OnKillCounterClick ->
            case List.reverse counters of
                [] ->
                    ( model, [], Cmd.none )

                c :: _ ->
                    ( model, [ KillCounter c ], Cmd.none )

        KilledCounter killedPid ->
            let
                updatedCounted =
                    List.filter (not << equals killedPid) counters
            in
            ( Counters pid updatedCounted, [], Cmd.none )


view :
    (MsgIn -> msg)
    -> Model
    -> (PID -> Html msg)
    -> Html msg
view wrap (Counters pid counters) renderPid =
    div []
        [ h1 [] [ text "Counters" ]
        , p [] [ text <| "(PID: " ++ pidToString pid ++ ")" ]
        , p []
            [ button [ onClick <| wrap OnKillCounterClick ] [ text "Kill first Counter" ]
            ]
        , button [ onClick <| wrap OnSpawnCounterClick ] [ text "Spawn a Counter" ]
        , table
            []
            ([ tr []
                [ th [] [ text "PID" ]
                , th [] [ text "Count" ]
                , th [] []
                ]
             ]
                ++ (List.map renderPid <| List.reverse counters)
            )
        ]

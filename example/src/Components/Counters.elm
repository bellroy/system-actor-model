module Components.Counters exposing (Model, MsgIn(..), MsgOut(..), component)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import System.Component.Layout exposing (Layout)
import System.Event exposing (ignoreAll)
import System.Process exposing (PID, equals, pidToString)


type Model
    = Counters PID InitialValue (List PID)


type alias InitialValue =
    Int


type MsgIn
    = ReceiveCounter PID
    | OnSpawnCounterClick InitialValue
    | OnKillCounterClick
    | KilledCounter PID


type MsgOut
    = SpawnCounter InitialValue
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
    ( PID, Decode.Value )
    -> ( Model, List MsgOut, Cmd MsgIn )
init ( pid, flags ) =
    case Decode.decodeValue Decode.int flags of
        Ok initialValue ->
            ( Counters pid initialValue [], [], Cmd.none )

        Err _ ->
            ( Counters pid 0 [], [], Cmd.none )


update :
    MsgIn
    -> Model
    -> ( Model, List MsgOut, Cmd MsgIn )
update msgIn ((Counters pid initialValue counters) as model) =
    case msgIn of
        ReceiveCounter counterPid ->
            ( Counters pid initialValue (counterPid :: counters), [], Cmd.none )

        OnSpawnCounterClick int ->
            ( model, [ SpawnCounter int ], Cmd.none )

        OnKillCounterClick ->
            case List.reverse counters of
                [] ->
                    ( model, [], Cmd.none )

                c :: _ ->
                    ( model, [ KillCounter c ], Cmd.none )

        KilledCounter killedPid ->
            let
                updatedCounters =
                    List.filter (not << equals killedPid) counters
            in
            ( Counters pid initialValue updatedCounters, [], Cmd.none )


view :
    (MsgIn -> msg)
    -> Model
    -> (PID -> Html msg)
    -> Html msg
view wrap (Counters pid initialValue counters) renderPid =
    div []
        [ h2 [] [ text ("Counters (PID: " ++ pidToString pid ++ ")") ]
        , div [ class "container clearfix" ]
            [ div [ class "row float-right" ]
                [ div [ class "btn-group", style "margin-bottom" "20px" ]
                    [ button
                        [ class "btn btn-secondary"
                        , onClick <| wrap OnKillCounterClick
                        ]
                        [ text "Kill first Counter"
                        ]
                    , button
                        [ class "btn btn-primary"
                        , onClick <| wrap (OnSpawnCounterClick initialValue)
                        ]
                        [ text "Spawn a Counter"
                        ]
                    ]
                ]
            ]
        , div [ class "container" ]
            [ div [ class "row" ]
                [ table
                    [ class "table" ]
                    [ thead [ class "thead-dark" ]
                        [ tr []
                            [ th [] [ text "PID" ]
                            , th [] [ text "Count" ]
                            , th [] []
                            ]
                        ]
                    , tbody [] (List.map renderPid <| List.reverse counters)
                    ]
                ]
            ]
        ]

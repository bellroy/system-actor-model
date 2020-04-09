module Components.Counters exposing (Model, MsgIn(..), MsgOut(..), component)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Decode
import System.Component.Layout exposing (Layout)
import System.Event as SystemEvent
import System.Process as PID exposing (PID)


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


component : Layout (Html msg) Model MsgIn MsgOut msg
component =
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    , events = SystemEvent.ignoreAll
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
                    List.filter (not << PID.equals killedPid) counters
            in
            ( Counters pid initialValue updatedCounters, [], Cmd.none )


view :
    (MsgIn -> msg)
    -> Model
    -> (PID -> Maybe (Html msg))
    -> Html msg
view toSelf (Counters pid initialValue counters) renderPid =
    Html.div []
        [ Html.h2 [] [ Html.text ("Counters (PID: " ++ PID.pidToString pid ++ ")") ]
        , Html.div [ HtmlA.class "container clearfix" ]
            [ Html.div [ HtmlA.class "row float-right" ]
                [ Html.div [ HtmlA.class "btn-group", HtmlA.style "margin-bottom" "20px" ]
                    [ Html.button
                        [ HtmlA.class "btn btn-secondary"
                        , HtmlE.onClick <| toSelf OnKillCounterClick
                        ]
                        [ Html.text "Kill first Counter"
                        ]
                    , Html.button
                        [ HtmlA.class "btn btn-primary"
                        , HtmlE.onClick <| toSelf (OnSpawnCounterClick initialValue)
                        ]
                        [ Html.text "Spawn a Counter"
                        ]
                    ]
                ]
            ]
        , Html.div [ HtmlA.class "container" ]
            [ Html.div [ HtmlA.class "row" ]
                [ Html.table
                    [ HtmlA.class "table" ]
                    [ Html.thead [ HtmlA.class "thead-dark" ]
                        [ Html.tr []
                            [ Html.th [] [ Html.text "PID" ]
                            , Html.th [] [ Html.text "Count" ]
                            , Html.th [] []
                            ]
                        ]
                    , List.reverse counters
                        |> List.filterMap renderPid
                        |> Html.tbody []
                    ]
                ]
            ]
        ]

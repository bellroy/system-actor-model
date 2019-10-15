module Components.Snackbar exposing (Model, MsgIn(..), MsgOut, component)

import Html exposing (..)
import Html.Attributes exposing (..)
import System.Component.Ui exposing (Ui)
import System.Event exposing (systemDefault)
import Time exposing (Posix, every)


type MsgIn
    = Tick Posix
    | NewSnack Snack


type MsgOut
    = NoMsgOut


type Model
    = Snackbar Snacks


type alias Snacks =
    List ( Int, Snack )


type alias Snack =
    { message : String
    }


component : Ui Model MsgIn MsgOut
component =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , events = systemDefault
    , view = view
    }


init : a -> ( Model, List MsgOut, Cmd MsgIn )
init _ =
    ( Snackbar []
    , []
    , Cmd.none
    )


update :
    MsgIn
    -> Model
    -> ( Model, List MsgOut, Cmd MsgIn )
update msg (Snackbar snacks) =
    case msg of
        Tick _ ->
            case snacks of
                ( 1, _ ) :: restOfSnacks ->
                    ( Snackbar restOfSnacks, [], Cmd.none )

                ( int, snack ) :: restOfSnacks ->
                    ( Snackbar (( int - 1, snack ) :: restOfSnacks), [], Cmd.none )

                [] ->
                    ( Snackbar [], [], Cmd.none )

        NewSnack snack ->
            ( Snackbar (snacks ++ [ ( 3, snack ) ])
            , []
            , Cmd.none
            )


subscriptions : Model -> Sub MsgIn
subscriptions (Snackbar snacks) =
    if List.isEmpty snacks then
        Sub.none

    else
        every 1000 Tick


view :
    Model
    -> Html MsgIn
view (Snackbar snacks) =
    case snacks of
        ( int, snack ) :: _ ->
            div [ class "fixed-bottom container" ]
                [ viewSnack int snack
                ]

        _ ->
            text ""


viewSnack :
    Int
    -> Snack
    -> Html MsgIn
viewSnack timeRemaining snack =
    div [ class "alert alert-info" ]
        [ text snack.message
        , text " ("
        , text <| String.fromInt timeRemaining
        , text ")"
        ]

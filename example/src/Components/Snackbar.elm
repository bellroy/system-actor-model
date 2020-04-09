module Components.Snackbar exposing (Model, MsgIn(..), component)

import Html exposing (Html)
import Html.Attributes as HtmlA
import System.Component.Ui exposing (Ui)
import System.Event as SystemEvent
import Time as Time exposing (Posix)


type MsgIn
    = Tick Posix
    | NewSnackMessage String


type Model
    = Snackbar Snacks


type alias Snacks =
    List ( Int, Snack )


type Snack
    = Snack Message


type Message
    = Message String


component : Ui (Html msg) Model MsgIn () msg
component =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , events = SystemEvent.systemDefault
    , view = view
    }


init : a -> ( Model, List (), Cmd MsgIn )
init _ =
    ( Snackbar []
    , []
    , Cmd.none
    )


update :
    MsgIn
    -> Model
    -> ( Model, List (), Cmd MsgIn )
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

        NewSnackMessage message ->
            ( Snackbar ((List.reverse <| List.take 2 (List.reverse snacks)) ++ [ ( 2, Snack (Message message) ) ])
            , []
            , Cmd.none
            )


subscriptions : Model -> Sub MsgIn
subscriptions (Snackbar snacks) =
    if List.isEmpty snacks then
        Sub.none

    else
        Time.every 1000 Tick


view :
    a
    -> Model
    -> Html msg
view _ (Snackbar snacks) =
    case snacks of
        ( int, snack ) :: _ ->
            Html.div [ HtmlA.class "fixed-bottom container" ]
                [ viewSnack int snack
                ]

        _ ->
            Html.text ""


viewSnack :
    Int
    -> Snack
    -> Html msg
viewSnack timeRemaining (Snack (Message message)) =
    Html.div [ HtmlA.class "alert alert-info" ]
        [ Html.text message
        , Html.text " ("
        , Html.text <| String.fromInt timeRemaining
        , Html.text ")"
        ]

module Components.Counter exposing (Model(..), MsgIn(..), MsgOut(..), component)

import Html as Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Decode
import System.Component.Ui exposing (Ui)
import System.Event as SystemEvent
import System.Process as PID exposing (PID)


{-| Our model will hold the current Count and the PID the System will assign to this component
-}
type Model
    = Counter PID Steps Count


type alias Count =
    Int


type alias Steps =
    Int


{-| Think of MsgIn as your Msg type in a classical Elm application
-}
type MsgIn
    = Increment
    | Decrement


{-| Think of MsgOut as Elm Cmds that you would like the System to run
-}
type MsgOut
    = LogCreated


{-| Setting up a component is much like setting up a Core Browser.element
-}
component : Ui (Html msg) Model MsgIn MsgOut msg
component =
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    , events = SystemEvent.systemDefault
    }


{-| When a component get's initialized you will receive it's assigned PID from the System
-}
init :
    ( PID, Decode.Value )
    -> ( Model, List MsgOut, Cmd MsgIn )
init ( pid, flags ) =
    let
        maybeCount =
            Decode.decodeValue Decode.int flags
                |> Result.toMaybe

        maybeValue =
            Decode.decodeValue (Decode.at [ "attributes", "value" ] Decode.string) flags
                |> Result.toMaybe
                |> Maybe.andThen String.toInt

        maybeSteps =
            Decode.decodeValue (Decode.at [ "attributes", "steps" ] Decode.string) flags
                |> Result.toMaybe
                |> Maybe.andThen String.toInt

        value =
            case ( maybeCount, maybeValue ) of
                ( Just _, Just a ) ->
                    a

                ( Just a, Nothing ) ->
                    a

                ( Nothing, Just a ) ->
                    a

                _ ->
                    0

        steps =
            Maybe.withDefault 1 maybeSteps
    in
    ( Counter pid steps value
    , [ LogCreated ]
    , Cmd.none
    )


update :
    MsgIn
    -> Model
    -> ( Model, List MsgOut, Cmd MsgIn )
update msgIn (Counter pid steps count) =
    case msgIn of
        Increment ->
            ( Counter pid steps (count + steps)
            , []
            , Cmd.none
            )

        Decrement ->
            ( Counter pid steps (count - steps)
            , []
            , Cmd.none
            )


view :
    (MsgIn -> msg)
    -> Model
    -> Html msg
view toSelf (Counter pid _ count) =
    Html.tr []
        [ Html.td [] [ Html.text (PID.pidToString pid) ]
        , Html.td []
            [ Html.strong [] [ Html.text (String.fromInt count) ]
            ]
        , Html.td []
            [ Html.div [ HtmlA.class "btn-group" ]
                [ Html.button
                    [ HtmlA.class "btn btn-primary"
                    , HtmlE.onClick <| toSelf Decrement
                    ]
                    [ Html.text "-" ]
                , Html.button
                    [ HtmlA.class "btn btn-primary"
                    , HtmlE.onClick <| toSelf Increment
                    ]
                    [ Html.text "+" ]
                ]
            ]
        ]
